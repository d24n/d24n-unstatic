package org.d24n.site

import scala.collection.*
import java.time.Instant
import java.nio.file.Path as JPath
import unstatic.UrlPath.*
import unstatic.*

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

trait Site extends ZTServerEndpointSource with StaticLocationBinding.Source:
  def serverUrl : Abs
  def basePath  : Rooted
  def sitePath  : Abs = serverUrl.reroot(basePath)

  def siteRoot = serverUrl.reroot( basePath )

  def serverRootedPath( fromSiteRootedPath : Rooted ) : Rooted = basePath.reroot( fromSiteRootedPath )
  def serverRootedPath( fromSiteRootedPath : String ) : Rooted = serverRootedPath( Rooted(fromSiteRootedPath) )

trait StaticResources[S <: Site] extends ZTServerEndpointSource with StaticLocationBinding.Source:
  val site : S

  def locationBindings : immutable.Seq[StaticLocationBinding]

  // Keys are site-rooted, but endpoints are server rooted!
  def endpointBindings: immutable.Seq[ZTEndpointBinding] =
    locationBindings.map { case StaticLocationBinding(siteRootedPath, source) => staticDirectoryServingEndpointBinding( siteRootedPath, site, source ) }

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

  def endpointBindings : immutable.Seq[ZTEndpointBinding]


private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def inLinkTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( (c : Char) => if ToDashChar(c) then '-' else c )





