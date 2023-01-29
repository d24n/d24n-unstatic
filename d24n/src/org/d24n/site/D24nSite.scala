package org.d24n.site

import scala.collection.*
import scala.util.Properties.lineSeparator as LineSep
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField
import java.nio.file.Path as JPath
import untemplate.Result
import zio.*
import unstatic.UrlPath.*
import unstatic.*
import unstatic.ztapir.*
import unstatic.ztapir.simple.*

object D24nSite extends ZTSite:
  object Frame:
    object Input:
      case class Main( renderLocation : SiteLocation, mainContentHtml : String )
      case class Article( renderLocation : SiteLocation, articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, permalinkLocation : SiteLocation, presentationMultiple : Boolean )
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

  object MainBlog extends SimpleBlog:
    override type Site = D24nSite.type
    override val site = D24nSite.this
    override val frontPage = Link.Inside.Home
    override val maxFrontPageEntries = 8
    override def entryUntemplates =
      val raw = IndexedUntemplates.filter { case (fqn, _) => fqn.indexOf(".mainblog.entry") >= 0 }.map( _(1) )
      raw.map( _.asInstanceOf[EntryUntemplate] ).toSet
    override def mediaPathPermalink( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
      MediaPathPermalink.yearMonthDayNameDir( checkable, ut )

    /**
     * Lays out only the entry, the fragment which will later become the main content of the page
     */
    override def layoutEntry(input: Layout.Input.Entry): String =
      layout_entry_html(input).text

    // overriding a def, but it's just a constant, so we override with lazy val
    override def entrySeparator : String =
      decorative.article_separator_html().text

    override def layoutPage(input: Layout.Input.Page): String =
      layout_main_html(input).text


object D24nSiteGenerator extends ZTSite.Static.Main(D24nSite)
