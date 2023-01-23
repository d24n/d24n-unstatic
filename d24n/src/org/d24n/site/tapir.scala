package org.d24n.site

import scala.reflect.Typeable

import sttp.tapir.*
import sttp.tapir.internal.RichEndpoint
import sttp.model.Method
import unstatic.UrlPath.*

val RootEndpoint = endpoint.get

def endpointForFixedPath( siteRootedPath : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  siteRootedPath.elements.foldLeft(RootEndpoint)( (accum, next) => accum.in(next) )

object AgnosticEndpointBinding:
  final case class Basic[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, -R](endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, _, R], serverLogic: INPUT => OUTPUT) extends AgnosticEndpointBinding[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]:
    lazy val staticallyGenerableFilePath = endpointStaticallyGenerableFilePath(endpoint)
  def apply[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, _, R], serverLogic: INPUT => OUTPUT) =
    Basic( endpoint, serverLogic )
trait AgnosticEndpointBinding[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, -R]:
  def endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, _, R]
  def serverLogic : INPUT => OUTPUT
  def staticallyGenerableFilePath : Option[String]

def endpointStaticallyGenerableFilePath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] ) : Option[String] =
  val inputs = endpoint.asVectorOfBasicInputs(includeAuth = true)
  val acceptableInputs = inputs.collect {
    case a : EndpointInput.FixedPath[_]                                                              => a
    case b @ EndpointInput.FixedMethod(Method(methodName),_,_) if methodName.equalsIgnoreCase("GET") => b
  }
  if (inputs.size != acceptableInputs.size) // we have some unacceptable inputs
    None
  else
    Some( inputs.collect{ case input : EndpointInput.FixedPath[_] => input.s }.mkString("/","/","") )
