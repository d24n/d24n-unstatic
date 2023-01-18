package org.d24n.site

import scala.collection.*
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField
import java.nio.file.Path as JPath
import com.mchange.sc.v3.failable.*

import unstatic.*

/*
enum ContentType( val mimeType : String ):
  case Html extends ContentType("text/html")
  case Markdown extends ContentType("text/markdown")
*/


// case class Page[S <: Site]( mainContentHtml : String, relPath : String, site : S )

// it would be great if we had compile time content-type info so that we'd fail
// on compile if our renderer wiring is bad, but it doesn't seem practical to use
// at the moment. Even if we parameterized metadatas by content type, we'd just have
// to cast everything to what we think it should be, as only convention binds indexed
// untemplates to what we expect of them.
// case class D24nMetadata( location : RelPath, contentType : ContentType, isEmbeddable : Boolean, site : D24nSite )

case class D24nMetadata()


// things that render fragments to output, usually HTML
type ContentRenderer =
  Function1[untemplate.Result[D24nMetadata], untemplate.Result[D24nMetadata]]

// type Framer = // input contentType should be text/html, isEmbeddable should be true
//   Function1[untemplate.Result[D24nMetadata], untemplate.Result[D24nMetadata]]

/*
enum FrameElement:
  case MainContent extends FrameElement
  case ArticleContent extends FrameElement // combine content, title, other metadata into a case class for framing?
  case ArticleTitle extends FrameElement

case class FrameInput( elems : immutable.Map[FrameElement,String], location : RelPath, site : Site )

object Framer:
  object Main:
    enum Key:
      case Content extends Key
    val Function : Framer[Framer.Main.Key] = main_frame_html
  object Article:
    enum Key:
      case Title extends Key    // String
      case Authors extends Key  // Seq[String]
      case PubDate extends Key  // ISO_INSTANCE
      case Tags extends Key     // Seq[String]
    val Function = ???
  object Articles:
    val Function = ???

type Framer[E] = Function1[immutable.Map[E,Any],untemplate.Result[D24nMetadata]]
*/

object Frame:
  object Input:
    case class Main( mainContentHtml : String )
    case class Article( articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant)
type Frame[E] = Function1[E,untemplate.Result[D24nMetadata]]


val ContentTypeBySuffix = immutable.Map (
  "html" -> "text/html",
  "md"   -> "text/markdown",
  "txt"  -> "text/plain",
)

val ContentRendererForContentType = immutable.Map[String,ContentRenderer] (
  "text/html" -> identity
)

// don't forget a compose template

trait Site:
  def serverUrl : AbsPath
  def basePath  : RelPath
  def sitePath  : AbsPath = serverUrl.resolve(basePath)

  def mbStaticResources : Option[JPath]

  //def serverRootPath( fromSiteRootPath : RelPath ) : RelPath = RelPath("/").resolve( serverUrl.relativize( sitePath.resolve( fromSiteRootPath ) ) )
  def serverRootPath( fromSiteRootPath : RelPath ) : RelPath = RelPath("/").resolve( basePath ).resolve( fromSiteRootPath )

// TODO: Generalize Entry.Info type
//       Build a Warned monad that collects warnings along the path
trait Blog[S <: Site, M]:
  object Entry:
    final case class Info(mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, contentType : String, mediaPath : RelPath, permalinkSiteRootPath : RelPath)
  final case class Entry(mediaPath : RelPath, site : S)
  type Untemplate = untemplate.Untemplate[Entry,M]
  def untemplates                                    : immutable.Vector[Untemplate]
  def entryInfo( template : Untemplate )             : Entry.Info
  def renderSingle( template : Untemplate )          : String
  // def renderLast( num : Int )                        : String
  // def renderRange( from : Instant, until : Instant ) : String

  // def renderSince( moment : Instant ) : String = renderRange( moment, Instant.now )

object D24nSite:
  class Exception( msg : String, cause : Throwable = null ) extends java.lang.Exception( msg, cause )
case class D24nSite (
  val serverUrl         : AbsPath,
  val basePath          : RelPath,
  val mbStaticResources : Option[JPath]
) extends Site:
  object Link:
    enum Inside(siteRootPath : RelPath):
      def serverRootPath = D24nSite.this.serverRootPath(siteRootPath)
      case Home extends Inside( RelPath("/") )
      case AboutUs extends Inside( RelPath("/about-us/") )
      case Donate extends Inside( RelPath("/donate/") )
    enum Outside( url : AbsPath ):
      case Apply extends Outside( AbsPath("https://docs.google.com/forms/d/e/1FAIpQLScBnYypFCEngFA4tc75_rUJLHbgUpcQPlMrZeRbCarGfxNNew/viewform") )


private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def inLinkTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( c => if ToDashChar(c) then '-' else c )

val MainSite : D24nSite = ???

val MainBlog : Blog[D24nSite,D24nMetadata] = new Blog[D24nSite,D24nMetadata]:
  val rawTemplates = Untemplates.filter { case (fqn, _) => fqn.indexOf(".mainblog.entry") >= 0 }
  val untemplates = rawTemplates.map( _.asInstanceOf[this.Untemplate] ).toVector
  def entryInfo( untemplate : this.Untemplate ) =
    val attrsLc = untemplate.UntemplateAttributes.map { case (k, v) => (k.toLowerCase, v) }.toMap
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
      val year  = pubDate.get(ChronoField.YEAR)
      val month = pubDate.get(ChronoField.MONTH_OF_YEAR)
      val day   = pubDate.get(ChronoField.DAY_OF_MONTH)
      val mediaPath = f"/$year%d/$month%2d/$day%2d/${inLinkTitle(title)}%s/"
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
    val (mediaPathStr, permalinkSiteRootPathStr) = mediaPathPermalink(pubDate, mbTitle.getOrElse("Untitled Post"))
    Entry.Info(mbTitle, authors, tags, pubDate, contentType, RelPath(mediaPathStr), RelPath(permalinkSiteRootPathStr))

  def renderSingle( untemplate : Untemplate ) : String =
    val info = entryInfo(untemplate)
    val renderer = ContentRendererForContentType(info.contentType)
    val entry = Entry(info.mediaPath, MainSite)
    val result = untemplate( entry )
    val renderResult = renderer(result)
    val articleFrameInput = Frame.Input.Article(renderResult.text, info.mbTitle, info.authors, info.tags, info.pubDate)
    val articleResult = frame_article_html(articleFrameInput)
    val mainFrameInput = Frame.Input.Main( articleResult.text )
    frame_main_html(mainFrameInput).text






