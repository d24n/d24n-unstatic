import mill._
import mill.scalalib._

// huge thanks to @lolgab onn the Scala discord!
import $file.buildCompilationSettings

// it would be better not to have a separate untemplate
// dependency except via unstatic.
//
// consider defining an 'UnstaticModule' that extends
// UntemplateModule, to ensure versions are sync'ed

import $ivy.`com.mchange::untemplate-mill:0.0.4`
import untemplate.mill._

val UnstaticVersion = "0.0.2"

object Dependency {
  val Unstatic             = ivy"com.mchange::unstatic:${UnstaticVersion}"
  val UnstaticZTapir       = ivy"com.mchange::unstatic-ztapir:${UnstaticVersion}"
}

object d24n extends UntemplateModule {
  override def scalaVersion = "3.2.1"

  // supports Scala 3.2.1
  override def ammoniteVersion = "2.5.6"

  override def scalacOptions = T{ Seq("-explain") }

  // we'll build an index!
  override def untemplateIndexNameFullyQualified : Option[String] = Some("org.d24n.site.IndexedUntemplates")

  override def untemplateSelectCustomizer: untemplate.Customizer.Selector = { key =>
    var out = untemplate.Customizer.empty

    // for everything
    out = out.copy(extraImports=out.extraImports :+ "org.d24n.site.*" :+ "unstatic.*")

    // imports and types for mainblog entries
    if (key.resolvedPackage.contains(".mainblog") && key.resolvedFunctionName.startsWith("layout_"))
        out = out.copy(extraImports=out.extraImports ++ Seq("D24nSite.MainBlog.Layout"))

    out
  }

  override def ivyDeps = T {
    super.ivyDeps() ++
      Agg (
        Dependency.Unstatic,
        Dependency.UnstaticZTapir,
      )
  }
}


