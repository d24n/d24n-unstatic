package org.d24n.site

import sttp.tapir.PublicEndpoint
import sttp.tapir.ztapir.*
import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import zio.http.{Http, HttpApp}
import zio.http.{Server, ServerConfig}
import zio.*

import unstatic.UrlPath.*


// modified from https://github.com/longliveenduro/zio-geolocation-tapir-tapir-starter/blob/b79c88b9b1c44a60d7c547d04ca22f12f420d21d/src/main/scala/com/tsystems/toil/Main.scala
val customServerOptions =
  ZioHttpServerOptions
    .customiseInterceptors
    .serverLog(
      DefaultServerLog[Task](
        doLogWhenReceived = msg => ZIO.succeed(println(msg)),
        doLogWhenHandled = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println( s"msg: ${msg}, err: ${err}" ))),
        doLogAllDecodeFailures = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println( s"msg: ${msg}, err: ${err}" ))),
        doLogExceptions = (msg: String, exc: Throwable) => ZIO.succeed(println(s"msg: ${msg}, exc: ${exc}")),
        noLog = ZIO.unit
      )
    )
    .options

val helloWorld: PublicEndpoint[String, Unit, String, Any] =
  endpoint.get
    .in("hello")
    .in(path[String]("name"))
    .out(stringBody)

// cribbing from https://github.com/softwaremill/tapir/blob/master/examples/src/main/scala/sttp/tapir/examples/HelloWorldZioHttpServer.scala
def buildServer( endpointSource: ZTServerEndpointSource ) =
  val app : HttpApp[Any,Throwable] =
    val endpointBindings = endpointSource.endpointBindings
    println(s"""endpointKeys:\n${endpointBindings.map( _(0) ).mkString("\n")}""")
    val endpoints = endpointBindings.map( _(1) )
    println(s"""endpoints:\n${endpoints.map(_.show).mkString("\n")}""")
    if (endpoints.isEmpty) throw new Exception("No endpoints defined.") //XXX: Better Exceptions
    def toHttp( endpoint : ZTServerEndpoint) = ZioHttpInterpreter(customServerOptions).toHttp( endpoint )
    // println( s"head: ${endpoints.head.show}" )
    // toHttp(endpoints.head) <> toHttp( helloWorld.zServerLogic(name => ZIO.succeed(s"Hello, $name!") ) )
    endpoints.tail.foldLeft(toHttp(endpoints.head))( (accum, next) => accum ++ toHttp(next) )
  app

object MainSiteHttpServer extends ZIOAppDefault {


  val app =
    buildServer(MainSite)
    // <> buildServer(MainSite)

  // starting the server
  override def run =
    val server = Server.serve(app)//.debug( "server" )
    server.provide(
      ServerConfig.live(ServerConfig.default.port(8999)),
      Server.live,
    ).exitCode
}