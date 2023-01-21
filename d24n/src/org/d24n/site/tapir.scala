package org.d24n.site

import sttp.tapir.*
import sttp.tapir.internal.RichEndpoint
import unstatic.RelPath

val RootEndpoint = endpoint.get

def endpointForFixedPath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( path : RelPath ) : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] = ???

object AgnosticEndpointBinding:
  final case class Basic[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, -R](endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R], serverLogic: INPUT => OUTPUT) extends AgnosticEndpointBinding[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
  def apply[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R](endpoint: Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R], serverLogic: INPUT => OUTPUT) =
    Basic( endpoint, serverLogic )
trait AgnosticEndpointBinding[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, -R]:
  def endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]
  def serverLogic : INPUT => OUTPUT

// TODO: Better exceptions
object FileGenerableAgnosticEndpointBinding:
  final case class Basic[ERROR_OUTPUT, OUTPUT, -R](endpoint: Endpoint[Unit, Unit, ERROR_OUTPUT, OUTPUT, R], serverLogic: Unit => OUTPUT) extends FileGenerableAgnosticEndpointBinding[ERROR_OUTPUT, OUTPUT, R]
  def apply[ERROR_OUTPUT, OUTPUT, R](endpoint: Endpoint[Unit, Unit, ERROR_OUTPUT, OUTPUT, R], serverLogic: Unit => OUTPUT) =
    Basic( endpoint, serverLogic )
trait FileGenerableAgnosticEndpointBinding[ERROR_OUTPUT, OUTPUT, -R] extends AgnosticEndpointBinding[Unit, Unit, ERROR_OUTPUT, OUTPUT, R]:
  val filePath =
    endpoint.asVectorOfBasicInputs()
      .collect {
        case input : EndpointInput.FixedPath[_] => input.s
        case _: EndpointInput.PathCapture[_] => throw new Exception("Can't have a variable path element in a file-generable endpoint!")
        case _: EndpointInput.PathsCapture[_] => throw new Exception("Can't have multiple variable path element in a file-generable endpoint!")
      }
      .mkString("/","/","")
