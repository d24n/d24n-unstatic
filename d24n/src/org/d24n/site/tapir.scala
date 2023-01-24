package org.d24n.site

import scala.collection.*
import sttp.tapir.ztapir.*
import sttp.tapir.{Endpoint,EndpointInput}
import sttp.tapir.internal.RichEndpoint
import sttp.model.{Header,MediaType,Method}
import sttp.tapir.server.ServerEndpoint
import unstatic.UrlPath.*
import zio.*

private def endpointForFixedPath( siteRootedPath : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  if (siteRootedPath == Rooted.root) then
    endpoint.get.in("")
  else
    siteRootedPath.elements.foldLeft(endpoint.get)( (accum, next) => accum.in( next ) )

type ZTServerEndpoint = ZServerEndpoint[Any,Any] //ServerEndpoint[Any,[t] =>> ZIO[Any,String,t]]

// Keys are site-rooted, but endpoints are server rooted!
trait ZTServerEndpointSource:
  def endpointBindings : immutable.Seq[Tuple2[Rooted,ZTServerEndpoint]]

def publicReadOnlyHtmlEndpointBinding( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ( Rooted, ZServerEndpoint[Any,Any] ) =
  siteRootedPath -> publicReadOnlyHtmlEndpoint( siteRootedPath, site, task )

def publicReadOnlyHtmlEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZServerEndpoint[Any,Any] =
  // XXX: Should I do something to break harder on non-nonFatal errors?
  val errMappedTask = task.mapError { t =>
    import java.io.*
    val sw = new StringWriter()
    t.printStackTrace(new PrintWriter(sw))
    sw.toString()
  }
  val endpoint =
    endpointForFixedPath(siteRootedPath.reroot(site.basePath))
      .errorOut(stringBody)
      .out(header(Header.contentType(MediaType.TextHtml)))
      .out(stringBody)
  endpoint.zServerLogic( _ => errMappedTask )

private def endpointStaticallyGenerableFilePath[R,F[_]]( serverEndpoint : ServerEndpoint[R,F] ) : Option[Rooted] =
  endpointStaticallyGenerableFilePath(serverEndpoint.endpoint)

private def endpointStaticallyGenerableFilePath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] ) : Option[Rooted] =
  val inputs = endpoint.asVectorOfBasicInputs(includeAuth = true)
  val acceptableInputs = inputs.collect {
    case a : EndpointInput.FixedPath[_]                                                              => a
    case b @ EndpointInput.FixedMethod(Method(methodName),_,_) if methodName.equalsIgnoreCase("GET") => b
  }
  if (inputs.size != acceptableInputs.size) // we have some unacceptable inputs
    None
  else
    Some( Rooted.fromElements( inputs.collect{ case input : EndpointInput.FixedPath[_] => input.s } : _* ) )
