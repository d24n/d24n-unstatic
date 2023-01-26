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
  val Failable             = ivy"com.mchange::failable:0.0.6"
  //val Tapir              = ivy"com.softwaremill.sttp.tapir::tapir-core:${TapirVersion}"
  val TapirZio             = ivy"com.softwaremill.sttp.tapir::tapir-zio:${TapirVersion}"
  val TapirZioHttpServer   = ivy"com.softwaremill.sttp.tapir::tapir-zio-http-server:${TapirVersion}"
}

object d24n extends UntemplateModule {
  override def scalaVersion = "3.2.1"

  // supports Scala 3.2.1
  override def ammoniteVersion = "2.5.6"

  // we'll build an index!
  override def untemplateIndexNameFullyQualified : Option[String] = Some("org.d24n.site.IndexedUntemplates")

  override def untemplateSelectCustomizer: untemplate.Customizer.Selector = { key =>
    var out = untemplate.Customizer.empty

    // imports and types for mainblog entries
    if (key.resolvedPackage.contains(".mainblog") && key.resolvedFunctionName.startsWith("entry"))
      out = out.copy(mbDefaultMetadataType = Some("D24nMetadata"), extraImports=out.extraImports :+ "org.d24n.site.*")

    // to customize, examine key and modify the customer
    // with out = out.copy=...
    //
    // e.g. out = out.copy(extraImports=Seq("d24n.*"))

    out
  }

  override def ivyDeps = T {
    super.ivyDeps() ++
      Agg (
        Dependency.Unstatic,
        Dependency.UnstaticZTapir,
        Dependency.Failable,
        Dependency.TapirZio,
        Dependency.TapirZioHttpServer,
      )
  }
}


