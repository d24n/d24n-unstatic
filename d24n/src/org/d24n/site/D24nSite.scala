package org.d24n.site

import scala.collection.*
import java.nio.file.Path as JPath
import java.net.URL
import unstatic.*
import unstatic.UrlPath.*
import unstatic.ztapir.*
import unstatic.ztapir.simple.*

object D24nSite extends ZTSite.Composite:
  object Link:
    enum Inside(siteRootedPath : Rooted) extends SiteLocation(siteRootedPath, D24nSite.this):
      case Home extends Inside( Rooted("/index.html") )
      case AboutUs extends Inside( Rooted("/about-us/index.html") )
      case Donate extends Inside( Rooted("/donate/index.html") )
      case Stylesheet extends Inside( Rooted("/css/style.css") )
      case Logo extends Inside(Rooted("/image/d24n-web-logo-x2.png"))
    enum Outside( val url : URL ):
      case Apply extends Outside( URL("https://docs.google.com/forms/d/e/1FAIpQLScBnYypFCEngFA4tc75_rUJLHbgUpcQPlMrZeRbCarGfxNNew/viewform") )

  override val serverUrl : Abs    = Abs("https://d24n.org/")
  override val basePath  : Rooted = Rooted("/d24n-test")

  // these had better be lazy, since at this point in the constructor StaticResources and MainBlog are null!
  override lazy val locationBindingSources : immutable.Seq[StaticLocationBinding.Source] = immutable.Seq( AllStaticResources )
  override lazy val endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]     = immutable.Seq( MainBlog, AllStaticResources )

  object AllStaticResources extends ZTStaticResources[D24nSite.type]:
    override val site = D24nSite.this
    override def locationBindings: immutable.Seq[StaticLocationBinding] =
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

    override def layoutEntry(input: Layout.Input.Entry): String = layout_entry_html(input).text

    // overriding a def, but it's just a constant, so we override with lazy val
    override lazy val entrySeparator : String = decorative.article_separator_html().text

    override def layoutPage(input: Layout.Input.Page): String = layout_main_html(input).text

object D24nSiteGenerator extends ZTSite.Static.Main(D24nSite)
