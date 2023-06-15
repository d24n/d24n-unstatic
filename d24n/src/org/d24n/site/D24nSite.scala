package org.d24n.site

import scala.collection.*
import java.nio.file.Path as JPath
import java.net.URL
import unstatic.*
import unstatic.UrlPath.*
import unstatic.ztapir.*
import unstatic.ztapir.simple.*

import unstatic.ztapir.simple.Attribute.Key.checkAttributeKey

import untemplate.*
import Untemplate.AnyUntemplate

import java.time.ZoneId

object D24nSite extends ZTSite.SingleRootComposite( JPath.of("d24n/static") ):
  object Link:
    enum Inside(siteRootedPath : Rooted) extends SiteLocation(siteRootedPath, D24nSite.this):
      case Home extends Inside( Rooted("/index.html") )
      case AboutUs extends Inside( Rooted("/about-us/index.html") )
      case Donate extends Inside( Rooted("/donate/index.html") )
      case Stylesheet extends Inside( Rooted("/css/style.css") )
      case RssFeed extends Inside( Rooted("/feed/index.rss") )
      case Logo extends Inside(Rooted("/image/d24n-web-logo-x2.png"))
    enum Decrypto(siteRootedPath : Rooted) extends SiteLocation(siteRootedPath, D24nSite.this):
      case Home extends Decrypto( Rooted("/decrypto/index.html") )
      case About extends Decrypto( Rooted("/decrypto/about/index.html") )
      case Stylesheet extends Decrypto( Rooted("/decrypto/css/style.css") )
      case RssFeed extends Decrypto( Rooted("/decrypto/feed/index.rss") )
    enum Outside( val url : URL ):
      case Apply extends Outside( URL("https://docs.google.com/forms/d/e/1FAIpQLScBnYypFCEngFA4tc75_rUJLHbgUpcQPlMrZeRbCarGfxNNew/viewform") )

  case class MainLayoutInput( renderLocation : SiteLocation, mbTitle : Option[String], mainContentHtml : String, sourceUntemplates : immutable.Seq[AnyUntemplate] = immutable.Seq.empty )

  // override val serverUrl : Abs    = Abs("https://d24n.org/")
  // override val basePath  : Rooted = Rooted.root

  // temporary values while developing on gh-pages:
  override val serverUrl : Abs    = Abs("https://d24n.github.io/")
  override val basePath  : Rooted = Rooted.root

  // should be lazy, since at this point in the constructor MainBlog and decrypto null!
  // avoid conflicts, but early items in the lists take precedence over later items
  override lazy val endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source] = immutable.Seq( MainBlog, MiscPageResources /*, Decrypto, DecryptoMiscPageResources */ )

  object MiscPageResources extends ZTEndpointBinding.Source:
    def task( renderLocation : SiteLocation, mbTitle : Option[String], generator : SiteLocation => untemplate.Result[Nothing]) = zio.ZIO.attempt {
      val mainContentHtml = generator(renderLocation).text
      layout_main_html(MainLayoutInput(renderLocation, mbTitle, mainContentHtml)).text
    }

    // Home is the blog front page, which MainBlog generates
    val AboutUsBinding = D24nSite.publicReadOnlyHtml(Link.Inside.AboutUs, task(Link.Inside.AboutUs, Some("d24n -- About Us"), page_about_us_html), None, immutable.Set("about","aboutUs","about-us"), false, true)
    val DonateBinding  = D24nSite.publicReadOnlyHtml(Link.Inside.Donate,  task(Link.Inside.Donate, Some("d24n -- Donate"), page_donate_html), None, immutable.Set("donate"), false, true)

    def endpointBindings : immutable.Seq[ZTEndpointBinding] = Vector(AboutUsBinding,DonateBinding)
  end MiscPageResources

  object DecryptoMiscPageResources extends ZTEndpointBinding.Source:
    def task( renderLocation : SiteLocation, mbTitle : Option[String], generator : SiteLocation => untemplate.Result[Nothing]) = zio.ZIO.attempt {
      val mainContentHtml = generator(renderLocation).text
      decrypto.layout_main_html(MainLayoutInput(renderLocation, mbTitle, mainContentHtml)).text
    }

    // Home is the blog front page, which MainBlog generates
    val AboutBinding = D24nSite.publicReadOnlyHtml(Link.Decrypto.About, task(Link.Decrypto.About, Some("about decrypto"), decrypto.page_about_decrypto_html), None, immutable.Set("about-decrypto"), false, true)

    def endpointBindings : immutable.Seq[ZTEndpointBinding] = Vector(AboutBinding)
  end DecryptoMiscPageResources

  private val MathJaxLowerCased = untemplate.LowerCased("MathJax")

  def needsMathJaxAny(untemplates : immutable.Seq[AnyUntemplate]) : Boolean =
    def needsIt( ut : AnyUntemplate ) : Boolean =
      ut.UntemplateAttributesLowerCased.get( MathJaxLowerCased ) match
        case Some( s : String  ) => s.equalsIgnoreCase("true")
        case Some( b : Boolean ) => b
        case _                   => false
    untemplates.exists( needsIt )
  end needsMathJaxAny

  // if we don't want MathJax to ever load on a multiple template view,
  // e.g for decrypto, where multiple views omit entry bodies
  def needsMathJaxOnly(untemplates : immutable.Seq[AnyUntemplate]) : Boolean =
    untemplates.size == 1 && needsMathJaxAny(untemplates)
  end needsMathJaxOnly
  
  trait D24nBlog extends SimpleBlog:
    def prefix : Rooted
    override type Site = D24nSite.type
    override val site = D24nSite.this
    override val timeZone = ZoneId.of("America/Los_Angeles")
    override def mediaPathPermalink( ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
      import MediaPathPermalink.*
      overridable( beneathPrefix(prefix)(yearMonthDayNameDir(timeZone)), ut )
    def extractMaybeTitle( sourceEntries : immutable.Seq[EntryResolved] ) : Option[String] =
      if sourceEntries.size == 1 then sourceEntries.head.checkAttributeKey(Attribute.Key.`Title`) else None
  end D24nBlog

  object MainBlog extends D24nBlog:
    override val prefix = Rooted.root
    override val feedTitle = "Decentralization Foundation Updates"
    override val frontPage = Link.Inside.Home
    override val frontPageIdentifiers = super.frontPageIdentifiers ++ immutable.Set("home","homePage") // since we are using the blog as home
    override val maxFrontPageEntries = None
    override lazy val rssFeed = Link.Inside.RssFeed
    override def entryUntemplates =
      IndexFilter.fromIndex( IndexedUntemplates )
        .inOrBeneathPackage("org.d24n.site.mainblog")
        .withNameLike( _.startsWith("entry_") )
        .untemplates
        .map( _.asInstanceOf[EntryUntemplate] )

    override def layoutEntry(input: Layout.Input.Entry): String = mainblog.layout_entry_html(input).text

    // overriding a def, but it's just a constant, so we override with val
    override val entrySeparator : String = decorative.article_separator_html().text

    // here the blog shares the sites main overall layout
    override def layoutPage(input: Layout.Input.Page): String =
      val mbTitle = extractMaybeTitle( input.sourceEntries )
      val mainLayoutInput = MainLayoutInput( input.renderLocation, mbTitle, input.mainContentHtml, input.sourceEntries.map( _.entryUntemplate ) )
      layout_main_html(mainLayoutInput).text
  end MainBlog

  object Decrypto extends D24nBlog:
    override val prefix = Rooted("/decrypto")
    override val feedTitle = "Decentralization Foundation -- Decrypto"
    override val frontPage = Link.Decrypto.Home
    override val frontPageIdentifiers = super.frontPageIdentifiers ++ immutable.Set("decrypto", "decrypto-home")
    override val maxFrontPageEntries = None
    override lazy val rssFeed = Link.Decrypto.RssFeed
    override def entryUntemplates =
      IndexFilter.fromIndex( IndexedUntemplates )
        .inOrBeneathPackage("org.d24n.site.decrypto")
        .withNameLike( _.startsWith("entry_") )
        .untemplates
        .map( _.asInstanceOf[EntryUntemplate] )

    override def layoutEntry(input: Layout.Input.Entry): String = decrypto.layout_entry_html(input).text

    // overriding defs, but we're supplying constants, so we override with vals
    override val renderMultiplePrologue : String = decrypto.render_multiple_prologue_html().text
    override val entrySeparator : String         = decrypto.multiple_entry_separator_html().text
    override val renderMultipleEpilogue : String = decrypto.render_multiple_epilogue_html().text

    // here the blog shares the sites main overall layout
    override def layoutPage(input: Layout.Input.Page): String =
      val mbTitle = extractMaybeTitle( input.sourceEntries )
      val mainLayoutInput = MainLayoutInput( input.renderLocation, mbTitle, input.mainContentHtml, input.sourceEntries.map( _.entryUntemplate ) )
      decrypto.layout_main_html(mainLayoutInput).text
  end Decrypto

// object D24nSiteGenerator extends ZTSite.Dynamic.Main(D24nSite)
object D24nSiteGenerator extends ZTMain(D24nSite, "d24n-site")
