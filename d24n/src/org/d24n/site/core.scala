package org.d24n.site

import scala.collection.*
import unstatic.UrlPath.*
import unstatic.ztapir.ZTSite

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






