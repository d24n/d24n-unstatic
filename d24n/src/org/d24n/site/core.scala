package org.d24n.site

import scala.collection.*
import scala.util.Properties.{lineSeparator => LineSep}

import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField
import java.nio.file.Path as JPath
import com.mchange.sc.v3.failable.*
import untemplate.Result

import zio.*

import unstatic.UrlPath.*

case class D24nMetadata()


// things that render fragments to output, usually HTML
type ContentRenderer =
  Function1[untemplate.Result[D24nMetadata], untemplate.Result[D24nMetadata]]

val ContentTypeBySuffix = immutable.Map (
  "html" -> "text/html",
  "md"   -> "text/markdown",
  "txt"  -> "text/plain",
)

val ContentRendererForContentType = immutable.Map[String,ContentRenderer] (
  "text/html" -> identity
)

// don't forget a compose template

trait Site extends ZTServerEndpointSource:
  def serverUrl : Abs
  def basePath  : Rooted
  def sitePath  : Abs = serverUrl.reroot(basePath)

  // def staticResources : immutable.Seq[Tuple2[Rooted,JPath]]

  def siteRoot = serverUrl.reroot( basePath )

  //def serverRootPath( fromSiteRootPath : RelPath ) : RelPath = RelPath("/").resolve( serverUrl.relativize( sitePath.resolve( fromSiteRootPath ) ) )
  def serverRootedPath( fromSiteRootedPath : Rooted ) : Rooted = basePath.reroot( fromSiteRootedPath )

  // Keys are site-rooted, but endpoints are server rooted!
  def endpointBindings : immutable.Seq[Tuple2[Rooted,ZTServerEndpoint]]

trait StaticResources[S <: Site] extends ZTServerEndpointSource:
  val site : S

  def locations : immutable.Seq[Tuple2[Rooted,JPath]]

  // Keys are site-rooted, but endpoints are server rooted!
  def endpointBindings: immutable.Seq[Tuple2[Rooted, ZTServerEndpoint]] =
    locations.map { case (rooted, jpath) => staticDirectoryServingEndpointBinding( rooted, site, jpath ) }

// TODO: Generalize Entry.Info type
//       Build a Warned monad that collects warnings along the path
trait Blog[S <: Site, M] extends ZTServerEndpointSource:
  object Entry:
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      contentType : String,
      mediaPath : Rooted, // from Site root
      permalinkSiteRootedPath : Rooted // from SiteRoot
    )
    object Resolved:
      given Ordering[Resolved] = Ordering.by( (r : Resolved) => (r.info.pubDate, r.untemplate.UntemplatePackage, r.untemplate.UntemplateName) ).reverse
    final case class Resolved( untemplate : Blog.this.Untemplate, info : Entry.Info )
  final case class Entry(mediaPathSiteRooted : Rooted, presentationMultiple : Boolean, site : S)
  type Untemplate = untemplate.Untemplate[Entry,M]
  val site : S
  def untemplates                                                               : immutable.Vector[Untemplate]
  def entryInfo( template : Untemplate )                                        : Entry.Info
  def renderSingle( template : Entry.Resolved, presentationMultiple : Boolean ) : String
  def renderLast( num : Int )                                                   : String
  def renderRange( from : Instant, until : Instant ) : String

  def renderSince( moment : Instant ) : String = renderRange( moment, Instant.now )

  def endpointBindings : immutable.Seq[Tuple2[Rooted,ZTServerEndpoint]]

object D24nSite:
  object Frame:
    object Input:
      case class Main( mainContentHtml : String, site : D24nSite )
      case class Article( articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, presentationMultiple : Boolean, site : D24nSite )
  type Frame[E] = Function1[E,untemplate.Result[D24nMetadata]]
  class Exception( msg : String, cause : Throwable = null ) extends java.lang.Exception( msg, cause )
case class D24nSite (
  val serverUrl               : Abs,
  val basePath                : Rooted,
  val mbStaticResources       : Option[JPath]
) extends Site:
  object Link:
    enum Inside(siteRootedPath : Rooted):
      def serverRootedPath = D24nSite.this.serverRootedPath(siteRootedPath)
      case Home extends Inside( Rooted("/") )
      case AboutUs extends Inside( Rooted("/about-us/") )
      case Donate extends Inside( Rooted("/donate/") )
      case Stylesheet extends Inside( Rooted("/css/style.css") )
    enum Outside( val url : Abs ):
      case Apply extends Outside( Abs("https://docs.google.com/forms/d/e/1FAIpQLScBnYypFCEngFA4tc75_rUJLHbgUpcQPlMrZeRbCarGfxNNew/viewform") )

  val MainBlog        = new D24nTopBlog(this)
  val StaticResources = D24nStaticResources(this)

  val bindingSources = immutable.Seq( MainBlog, StaticResources )

  def endpointBindings : immutable.Seq[Tuple2[Rooted,ZTServerEndpoint]] = bindingSources.flatMap( _.endpointBindings )

private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def inLinkTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( (c : Char) => if ToDashChar(c) then '-' else c )

class D24nStaticResources( val site : D24nSite ) extends StaticResources[D24nSite]:
  def locations : immutable.Seq[Tuple2[Rooted,JPath]] =
    Vector("wp-content","css","font","image")
      .map( dir => ( Rooted.fromElements(dir) -> JPath.of("d24n/static", dir) ) )

class D24nTopBlog( val site : D24nSite ) extends Blog[D24nSite,D24nMetadata]:
  val rawTemplates = Untemplates.filter { case (fqn, _) => fqn.indexOf(".mainblog.entry") >= 0 }.map( _(1) )
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

    def mediaPathPermalink( pubDate : Instant, title : String ) : (String, String) =
      val zoned = pubDate.atZone(ZoneId.systemDefault())
      val year  = zoned.get(ChronoField.YEAR)
      val month = zoned.get(ChronoField.MONTH_OF_YEAR)
      val day   = zoned.get(ChronoField.DAY_OF_MONTH)
      val mediaPath = f"/$year%d/$month%02d/$day%02d/${inLinkTitle(title)}%s/"
      ( mediaPath, mediaPath + "index.html" )

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
    val (mediaPathStr, permalinkSiteRootedPathStr) = mediaPathPermalink(pubDate, mbTitle.getOrElse("Untitled Post"))
    Entry.Info(mbTitle, authors, tags, pubDate, contentType, Rooted(mediaPathStr), Rooted(permalinkSiteRootedPathStr))

  private def mainFrame( fragmentText : String ) : String =
    val mainFrameInput = D24nSite.Frame.Input.Main(fragmentText, site)
    frame_main_html(mainFrameInput).text

  private def renderSingleFragment( resolved : Entry.Resolved, presentationMultiple : Boolean ) : untemplate.Result[D24nMetadata] =
    val Entry.Resolved(untemplate, info) = resolved
    val renderer = ContentRendererForContentType(info.contentType)
    val entry = Entry(info.mediaPath, presentationMultiple, site)
    val result = untemplate(entry)
    val renderResult = renderer(result)
    val articleFrameInput = D24nSite.Frame.Input.Article(renderResult.text, info.mbTitle, info.authors, info.tags, info.pubDate, presentationMultiple, site)
    frame_article_html(articleFrameInput)

  def renderSingle( resolved : Entry.Resolved, presentationMultiple : Boolean ) : String =
    val articleResult = renderSingleFragment( resolved, presentationMultiple )
    mainFrame( articleResult.text )

  private def renderResolveds( ssr : immutable.SortedSet[Entry.Resolved] ) : String =
    val fragmentTexts = ssr.map(resolved => renderSingleFragment(resolved, true).text)
    val unifiedFragmentText = fragmentTexts.mkString(decorative.article_separator_html().text)
    mainFrame(unifiedFragmentText)

  def renderLast( num : Int ) : String =
    val rs = resolveds.take(num)
    renderResolveds( rs )

  def renderRange( from : Instant, until : Instant ) : String =
    val ordering = summon[Ordering[Instant]]
    val rs = resolveds.filter( r => ordering.gteq(from,r.info.pubDate) && ordering.lt(r.info.pubDate, until) )
    renderResolveds( rs )

  def endpointBindings : immutable.Seq[Tuple2[Rooted,ZTServerEndpoint]] =
    val permalinks =  resolveds.to(Vector)
      .map { r =>
        publicReadOnlyHtmlEndpointBinding(r.info.permalinkSiteRootedPath, site, ZIO.attempt( renderSingle(r, false)))
      }
    permalinks :+ publicReadOnlyHtmlEndpointBinding( Rooted.root, site, ZIO.attempt( renderLast(10) ) )

val MainSite : D24nSite = new D24nSite(Abs("https://d24n.org/"), Rooted.root, None )






