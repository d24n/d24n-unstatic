package org.d24n.site

import scala.collection.*
import scala.util.Properties.lineSeparator as LineSep
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField
import java.nio.file.Path as JPath
import com.mchange.sc.v3.failable.*
import untemplate.Result
import zio.*
import unstatic.UrlPath.*
import unstatic.*
import unstatic.ztapir.*

object D24nSite extends ZTSite:
  object Frame:
    object Input:
      case class Main( renderLocation : SiteLocation, mainContentHtml : String, site : D24nSite.type )
      case class Article( renderLocation : SiteLocation, articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, permalinkLocation : SiteLocation, presentationMultiple : Boolean, site : D24nSite.type )
  type Frame[E] = Function1[E,untemplate.Result[D24nMetadata]]
  object Link:
    enum Inside(siteRootedPath : Rooted) extends SiteLocation(siteRootedPath, D24nSite.this):
      case Home extends Inside( Rooted("/index.html") )
      case AboutUs extends Inside( Rooted("/about-us/index.html") )
      case Donate extends Inside( Rooted("/donate/index.html") )
      case Stylesheet extends Inside( Rooted("/css/style.css") )
      case Logo extends Inside(Rooted("/image/d24n-web-logo-x2.png"))
    enum Outside( val url : Abs ):
      case Apply extends Outside( Abs("https://docs.google.com/forms/d/e/1FAIpQLScBnYypFCEngFA4tc75_rUJLHbgUpcQPlMrZeRbCarGfxNNew/viewform") )

  class Exception( msg : String, cause : Throwable = null ) extends java.lang.Exception( msg, cause )

  val serverUrl : Abs    = Abs("https://d24n.org/")
  val basePath  : Rooted = Rooted("/d24n-test")

  // these had better be lazy, since at this point in the constructor StaticResources and MainBlog are null!
  lazy val locationSources : immutable.Seq[StaticLocationBinding.Source] = immutable.Seq( StaticResources )
  lazy val bindingSources  : immutable.Seq[ZTEndpointBinding.Source]     = immutable.Seq( MainBlog, StaticResources )

  def locationBindings : immutable.Seq[StaticLocationBinding] = locationSources.flatMap( _.locationBindings )

  def endpointBindings : immutable.Seq[ZTEndpointBinding] = bindingSources.flatMap( _.endpointBindings )

  val StaticResources = new ZTStaticResources[D24nSite.type]:
    val site = D24nSite.this
    def locationBindings: immutable.Seq[StaticLocationBinding] =
      Vector("wp-content","css","font","image")
        .map( dir => StaticLocationBinding( Rooted.fromElements(dir), JPath.of("d24n/static", dir) ) )

  val MainBlog : ZTBlog[D24nSite.type,D24nMetadata] = new ZTBlog[D24nSite.type,D24nMetadata]:
    val site = D24nSite.this
    val rawTemplates = IndexedUntemplates.filter { case (fqn, _) => fqn.indexOf(".mainblog.entry") >= 0 }.map( _(1) )
    val untemplates = rawTemplates.map( _.asInstanceOf[this.Untemplate] ).toVector

    // reverse-chronological!
    val resolveds = untemplates.map( ut => Entry.Resolved(ut, entryInfo(ut)) ).to(immutable.SortedSet)

    def entryInfo( untemplate : this.Untemplate ) =
      val attrsLc = untemplate.UntemplateAttributes.map { case (k, v) => (k.toLowerCase, v) }
      def getMaybeMultiple(keySingular : String) : Seq[String] =
        attrsLc.get(keySingular + "s") match
          case Some(seq: Seq[_]) => seq.map( _.toString ) // to avoid unchecked Seq[String] match
          case Some(str: String) => str.split(",").map(_.trim).toSeq
          case Some( other ) => throw new D24nSite.Exception(s"Unexpected '${keySingular}s' type: ${other}")
          case None =>
            attrsLc.get(keySingular) match
              case Some(str: String) => Seq(str)
              case Some( other ) => throw new D24nSite.Exception(s"Unexpected '${keySingular}' type: ${other}")
              case None => Nil

      def parseTimestampIsoInstant( timestamp : String ) : Failable[Instant] =
        for
          temporalAccessor <- Failable( ISO_INSTANT.parse(timestamp) )
        yield
          Instant.from(temporalAccessor)

      def parseTimestampFromIsoLocalDate( timestamp : String ) : Failable[Instant] =
        for
          temporalAccessor <- Failable( ISO_LOCAL_DATE.parse(timestamp)  )
          ld               <- Failable( LocalDate.from(temporalAccessor) )
          zdt              <- Failable( ld.atTime(12,0).atZone(ZoneId.systemDefault()) )
        yield
          Instant.from(zdt)

      def parseTimestamp( timestamp : String ) : Instant =
        (parseTimestampIsoInstant(timestamp) orElseTrace parseTimestampFromIsoLocalDate(timestamp)).assert

      def contentTypeFromSuffix( name : String ) : Option[String] =
        val suffixDelimiter = name.lastIndexOf("_")
        if suffixDelimiter >= 0 then
          val suffix = name.substring(suffixDelimiter + 1)
          ContentTypeBySuffix.get(suffix)
        else
          None

      val FilePathChars = immutable.Set('/','\\', ':')
      def ensureNoFilePathChars( s : String ) =
        assert(!s.exists(FilePathChars.apply), s"File path characters not permitted: ${s}")

      def mediaPathPermalink( mbPermalink : Option[String], pubDate : Instant, title : String, mbLinkName : Option[String] ) : (Rooted, Rooted) =
        mbPermalink match
          case Some( permalink ) =>
            val pl = Rooted.parseAndRoot(permalink)
            ( pl.parent, pl )
          case None =>
            val zoned = pubDate.atZone(ZoneId.systemDefault())
            val year  = zoned.get(ChronoField.YEAR)
            val month = zoned.get(ChronoField.MONTH_OF_YEAR)
            val day   = zoned.get(ChronoField.DAY_OF_MONTH)
            val linkName = mbLinkName.getOrElse(linkableTitle(title))
            ensureNoFilePathChars(linkName)
            val mediaPath = f"/$year%d/$month%02d/$day%02d/${linkName}%s/"
            ( Rooted(mediaPath), Rooted(mediaPath + "index.html") )

      val mbTitle = attrsLc.get("title").map( _.toString )
      val authors = getMaybeMultiple("author")
      val tags    = getMaybeMultiple("tag")
      val pubDate =
        attrsLc.get("pubdate") orElse attrsLc.get("publicationdate") match
          case Some( instant : Instant )  => instant
          case Some( timestamp : String ) => parseTimestamp( timestamp.trim )
          case Some( other )              => throw new D24nSite.Exception(s"Unexpected publication date format: ${other}")
          case None                       => throw new D24nSite.Exception(s"PubDate or PublicationDate attribute required, not found.")
      val contentType =
        (attrsLc.get("content-type").map( _.toString ) orElse contentTypeFromSuffix(untemplate.UntemplateName)).getOrElse("text/plain")
      val mbPermalink = attrsLc.get("permalink").map( _.toString )
      val mbLinkName = attrsLc.get("linkname").map( _.toString )
      val (mediaPath, permalinkSiteRootedPath) = mediaPathPermalink(mbPermalink, pubDate, mbTitle.getOrElse("Untitled Post"), mbLinkName)
      Entry.Info(mbTitle, authors, tags, pubDate, contentType, mediaPath, permalinkSiteRootedPath)

    private def mainFrame( renderLocation : SiteLocation, fragmentText : String ) : String =
      val mainFrameInput = D24nSite.Frame.Input.Main(renderLocation, fragmentText, site)
      frame_main_html(mainFrameInput).text

    private def renderSingleFragment( renderLocation : SiteLocation, resolved : Entry.Resolved, presentationMultiple : Boolean ) : untemplate.Result[D24nMetadata] =
      val Entry.Resolved(untemplate, info) = resolved
      val renderer = ContentRendererForContentType(info.contentType)
      val entry = Entry(info.mediaPath, presentationMultiple, site)
      val result = untemplate(entry)
      val renderResult = renderer(result)
      val articleFrameInput = D24nSite.Frame.Input.Article(renderLocation, renderResult.text, info.mbTitle, info.authors, info.tags, info.pubDate, SiteLocation(info.permalinkSiteRootedPath,D24nSite.this), presentationMultiple, site)
      frame_article_html(articleFrameInput)

    def renderSingle( renderLocation : SiteLocation, resolved : Entry.Resolved, presentationMultiple : Boolean ) : String =
      val articleResult = renderSingleFragment(renderLocation, resolved, presentationMultiple )
      mainFrame( renderLocation, articleResult.text )

    private def renderResolveds( renderLocation : SiteLocation, ssr : immutable.SortedSet[Entry.Resolved] ) : String =
      val fragmentTexts = ssr.to(List).map(resolved => renderSingleFragment(renderLocation, resolved, true).text)
      val unifiedFragmentText = fragmentTexts.mkString(decorative.article_separator_html().text)
      mainFrame(renderLocation, unifiedFragmentText)

    def renderLast( renderLocation : SiteLocation, num : Int ) : String =
      val rs = resolveds.take(num)
      renderResolveds( renderLocation, rs )

    def renderRange( renderLocation : SiteLocation, from : Instant, until : Instant ) : String =
      val ordering = summon[Ordering[Instant]]
      val rs = resolveds.filter( r => ordering.gteq(from,r.info.pubDate) && ordering.lt(r.info.pubDate, until) )
      renderResolveds( renderLocation, rs )

    def endpointBindings : immutable.Seq[ZTEndpointBinding] =
      val permalinks =  resolveds.to(Vector)
        .map { r =>
          val renderLocation = SiteLocation(r.info.permalinkSiteRootedPath, D24nSite.this)
          ZTEndpointBinding.publicReadOnlyHtml(renderLocation, site, ZIO.attempt( renderSingle(renderLocation, r, false)))
        }
      permalinks :+ ZTEndpointBinding.publicReadOnlyHtml( Link.Inside.Home, site, ZIO.attempt( renderLast(Link.Inside.Home, 10) ) )


object D24nSiteGenerator extends ZTSite.Static.Main(D24nSite)
