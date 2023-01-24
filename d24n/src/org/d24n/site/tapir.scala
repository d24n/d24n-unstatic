package org.d24n.site

import scala.collection.*
import sttp.tapir.ztapir.*
import sttp.tapir.{Endpoint,EndpointInput,EndpointIO}
import sttp.tapir.internal.RichEndpoint
import sttp.model.{Header,MediaType,Method}
import sttp.tapir.server.ServerEndpoint
import unstatic.UrlPath.*
import zio.*

import java.nio.file.Path as JPath


private def endpointForFixedPath( serverRootedPath : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  if (serverRootedPath == Rooted.root) then
    endpoint.get.in("")
  else
    serverRootedPath.elements.foldLeft(endpoint.get)( (accum, next) => accum.in( next ) )

private def inputsForFixedPath( serverRootedPath : Rooted ) : EndpointInput[Unit] =
  if (serverRootedPath == Rooted.root) then
    "" : EndpointInput[Unit]
  else
    serverRootedPath.elements.tail.foldLeft(serverRootedPath.elements.head : EndpointInput[Unit])( (accum, next) => accum / next )

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

def staticDirectoryServingEndpointBinding( siteRootedPath: Rooted, site: Site, dir : JPath ) : ( Rooted, ZServerEndpoint[Any,Any] ) =
  siteRootedPath -> staticDirectoryServingEndpoint( siteRootedPath, site, dir )

def staticDirectoryServingEndpoint( siteRootedPath: Rooted, site: Site, dir : JPath ) : ZServerEndpoint[Any,Any] =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  filesGetServerEndpoint[Task](inputsForFixedPath(serverRootedPath))(dir.toAbsolutePath.toString)

def endpointStaticallyGenerableFilePath[R,F[_]]( serverEndpoint : ServerEndpoint[R,F] ) : Option[Rooted] =
  endpointStaticallyGenerableFilePath(serverEndpoint.endpoint)

def endpointStaticallyGenerableFilePath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] ) : Option[Rooted] =
  val inputs = endpoint.asVectorOfBasicInputs(includeAuth = true)
  val acceptableInputs = inputs.collect {
    case a : EndpointInput.FixedPath[_]                                                                           => a
    case b @ EndpointInput.FixedMethod(Method(methodName),_,_) if methodName.equalsIgnoreCase("GET") => b
    case c : EndpointIO.Empty[_]                                                                                  => c
  }
  if (inputs.size != acceptableInputs.size) // we have some unacceptable inputs
    // println("Unacceptable inputs: " + inputs.filter( inp => !acceptableInputs.contains(inp) ).mkString(", "))
    None
  else
    Some( Rooted.fromElements( inputs.collect{ case input : EndpointInput.FixedPath[_] => input.s } : _* ) )
