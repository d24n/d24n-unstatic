import mill._
import mill.define._
import mill.scalalib._
import mill.define.Source
import mill.modules.Jvm
import mill.api.Result

// huge thanks to @lolgab onn the Scala discord!
import $file.buildCompilationSettings

import $ivy.`com.mchange::untemplate-mill:0.0.2`
import untemplate.mill._

val TapirVersion = "1.2.6"

val UnstaticVersion = "0.0.1-SNAPSHOT"

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

    // imports and types for mainblog entries
    if (key.resolvedPackage.contains(".mainblog") && key.resolvedFunctionName.startsWith("entry"))
      out = out.copy(extraImports=out.extraImports :+ "org.d24n.site.*")

    if (key.resolvedFunctionName.startsWith("layout_"))
      out = out.copy(extraImports=out.extraImports ++ Seq("org.d24n.site.*","unstatic.*", "D24nSite.MainBlog.Layout"))

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


