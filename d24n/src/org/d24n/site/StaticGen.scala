package org.d24n.site

import scala.collection.*
import zio.*
import java.nio.file.{Files, StandardCopyOption, Path as JPath}
import unstatic.UrlPath.*


object StaticGen:
  case class Result( generated : immutable.Seq[Rooted], ignored : immutable.Seq[Rooted], ungenerable : immutable.Seq[Rooted])

  private def overwriteCopyRegularFile( source : JPath, dest : JPath ) = ZIO.attempt {
    val destParent = dest.getParent
    Files.createDirectories(destParent)
    Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING)
  }
  private def overwriteCopyDirectory(source : JPath, dest : JPath, ignoreRegularFile : JPath => Boolean = _ => false) = ZIO.attempt {
    import scala.jdk.StreamConverters.*

    val srcPaths = Files.walk(source).toScala(List).filter(p => !Files.isDirectory(p) && ignoreRegularFile(p))
    val destPaths = srcPaths.map(p => dest.resolve(source.relativize(p)))
    srcPaths.zip(destPaths).foreach { case (srcPath, destPath) =>
      if (Files.isDirectory(srcPath)) then Files.createDirectories(destPath)
      else Files.copy(srcPath, destPath, StandardCopyOption.REPLACE_EXISTING)
    }
  }
  private def checkIsDir(path : JPath) = ZIO.attempt( Files.isDirectory(path) )

  def generate(
    endpointBindings    : immutable.Seq[ZTEndpointBinding],
    staticLocationTups  : immutable.Seq[Tuple2[Rooted,JPath]],
    ignorePrefixes      : immutable.Seq[Rooted],
    genSiteRootDir      : JPath
  ) : Task[Result] =
    val (ignoredEndpointBindings, unignoredEndpointBindings)
      = endpointBindings.partition( epb => ignorePrefixes.exists(pfx => pfx.isPrefixOf(epb.siteRootedPath)) )

    val (ignoredLocationTups, unignoredLocationTups)
      = staticLocationTups.partition( slt => ignorePrefixes.exists(pfx => pfx.isPrefixOf(slt(0))) )

    val (ungenerableEndpointBindings, generableEndpointBindings)
      = unignoredEndpointBindings.partition( ep => ep.mbGenerator.isEmpty )

    val noExceptionResult =
      val generated = generableEndpointBindings.map( _.siteRootedPath )
      val ignored = ignoredEndpointBindings.map( _.siteRootedPath ) ++ ignoredLocationTups.map( _(0) )
      val ungenerable = ungenerableEndpointBindings.map( _.siteRootedPath ).filter( siteRootedPath => !ignored.contains(siteRootedPath) )
      Result( generated, ignored, ungenerable )

    def findDestPathFor(siteRootedPath: Rooted) = ZIO.attempt {
      if siteRootedPath == Rooted.root then
        genSiteRootDir
      else
        val elements = siteRootedPath.elements
        genSiteRootDir.resolve(JPath.of(elements.head, elements.tail: _*))
    }
    def generateLocation( siteRootedPath : Rooted, source : JPath ) : Task[Unit] =
      for
        destPath    <- findDestPathFor(siteRootedPath)
        sourceIsDir <- checkIsDir(source)
        _           <- if sourceIsDir then overwriteCopyDirectory( source, destPath ) else overwriteCopyRegularFile( source, destPath )
      yield ()
    def writeStringFor( siteRootedPath : Rooted, contents : String, codec : scala.io.Codec = scala.io.Codec.UTF8 ) : Task[Unit] =
      for
        destPath <- findDestPathFor(siteRootedPath)
        _        <- ZIO.attempt( Files.writeString(destPath, contents, codec.charSet) )
      yield()

    val locationTasks = unignoredLocationTups.map(generateLocation.tupled)
    val endpointTasks = generableEndpointBindings.map { case generable: ZTEndpointBinding =>
      for
        contents <- generable.mbGenerator.get // we've already verified this is non-empty, see above
        _ <- writeStringFor(generable.siteRootedPath, contents)
      yield ()
    }
    ZIO.foreachDiscard(locationTasks ++ endpointTasks)(identity).map( _ => noExceptionResult )
