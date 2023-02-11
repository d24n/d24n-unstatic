package org.d24n.site

import scala.collection.*
import java.nio.file.Path as JPath
import java.net.URL
import unstatic.*
import unstatic.UrlPath.*
import unstatic.ztapir.*
import unstatic.ztapir.simple.*

import untemplate.*

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

  case class MainLayoutInput( renderLocation : SiteLocation, mainContentHtml : String, sourceUntemplates : immutable.Seq[Untemplate.AnyUntemplate] = immutable.Seq.empty )

  // override val serverUrl : Abs    = Abs("https://d24n.org/")
  // override val basePath  : Rooted = Rooted.root

  // temporary values while developing on gh-pages:
  override val serverUrl : Abs    = Abs("https://d24n.github.io/")
  override val basePath  : Rooted = Rooted("/d24n-unstatic/")

  // these had better be lazy, since at this point in the constructor StaticResources and MainBlog are null!
  // note that static resources need to be included both as static locations and as endpoints, so that they can be
  // both generated and served!
  //
  // avoid conflicts, but...
  //   (1) early items in the lists take precedence over later items
  //   (2) endpoint bindings take precedence over location bindings
  //
  override lazy val locationBindingSources : immutable.Seq[StaticLocationBinding.Source] = immutable.Seq( RootStaticResource )
  override lazy val endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]     = immutable.Seq( MainBlog, MiscPageResources, RootStaticResource )

  object MiscPageResources extends ZTEndpointBinding.Source:
    def task( renderLocation : SiteLocation, generator : SiteLocation => untemplate.Result[Nothing]) = zio.ZIO.attempt {
      val mainContentHtml = generator(renderLocation).text
      layout_main_html(MainLayoutInput(renderLocation, mainContentHtml)).text
    }

    // Home is the blog front page, which MainBlog generates
    val AboutUsBinding = ZTEndpointBinding.publicReadOnlyHtml(Link.Inside.AboutUs, task(Link.Inside.AboutUs, page_about_us_html))
    val DonateBinding  = ZTEndpointBinding.publicReadOnlyHtml(Link.Inside.Donate,  task(Link.Inside.Donate, page_donate_html))

    def endpointBindings : immutable.Seq[ZTEndpointBinding] = Vector(AboutUsBinding,DonateBinding)

  object RootStaticResource extends ZTStaticResources[D24nSite.type]:
    override val site = D24nSite.this
    override def locationBindings: immutable.Seq[StaticLocationBinding] = List(StaticLocationBinding(Rooted.root, JPath.of("d24n/static")))

  object MainBlog extends SimpleBlog:
    override type Site = D24nSite.type
    override val site = D24nSite.this
    override lazy val rssFeed = site.location( "/feed/index.rss" )
    override val feedTitle = "Decentralization Foundation Updates"
    override val frontPage = Link.Inside.Home
    override val maxFrontPageEntries = None
    override def entryUntemplates =
      def isEntry( fqn : String ) =
        val asVec = fqn.split('.').toVector
        asVec.length > 1 && asVec.last.startsWith("entry") && asVec.contains("mainblog")
      val raw = IndexedUntemplates.filter { case (fqn, _) => isEntry(fqn) }.map( _(1) )
      raw.map( _.asInstanceOf[EntryUntemplate] ).toSet
    override def mediaPathPermalink( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
      import MediaPathPermalink.*
      overridable( yearMonthDayNameDir, ut )

    override def layoutEntry(input: Layout.Input.Entry): String = mainblog.layout_entry_html(input).text

    // overriding a def, but it's just a constant, so we override with val
    override val entrySeparator : String = decorative.article_separator_html().text

    // here the blog shares the sites main overall layout
    override def layoutPage(input: Layout.Input.Page): String =
      val mainLayoutInput = MainLayoutInput( input.renderLocation, input.mainContentHtml, input.sourceEntries.map( _.entryUntemplate ) )
      layout_main_html(mainLayoutInput).text

// object D24nSiteGenerator extends ZTSite.Dynamic.Main(D24nSite)
object D24nSiteGenerator extends ZTSite.Main(D24nSite, "d24n-site")
