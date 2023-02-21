package io.github.caeus.elodin.util

import zio.{RIO, Scope, ZIO}

import java.net.URI
import java.nio.file.spi.FileSystemProvider
import java.nio.file.{FileSystem, FileSystems}
import java.util.Collections
import scala.jdk.CollectionConverters.*
import scala.util.chaining.*

object DeepFS {

  private val defaultFs = FileSystems.getDefault
  val zipFsProvider: FileSystemProvider = FileSystemProvider
    .installedProviders()
    .asScala
    .toSeq
    .find(_.getClass.getCanonicalName.equals("jdk.nio.zipfs.ZipFileSystemProvider"))
    .getOrElse(
      throw new IllegalStateException("Current classloader doesn't have ZipFileSystemProvider")
    )
  private val supportedSchemes = Set("jar:file:", "file://")
  private def recFS(parts: List[String], currFs: FileSystem): RIO[Scope, FileSystem] = {
    parts.pipe {
      case Nil => ZIO.succeed(currFs)
      case head :: tail =>
        ZIO.acquireRelease(
          ZIO.attempt(zipFsProvider.newFileSystem(currFs.getPath(head), Collections.emptyMap))
        )(fs => ZIO.attempt(fs.close()).orDie)
          .flatMap { fs =>
            recFS(tail, fs)
          }
    }
  }
//TaskManaged
  def fromUri(uri: URI): RIO[Scope, FileSystem] = {
    val uriS = uri.toString
    supportedSchemes
      .find(uriS.startsWith)
      .map { scheme =>
        uriS.substring(scheme.length)
      }
      .map { path =>
        path.split("!").toList
      }
      .map { parts => recFS(parts, defaultFs) }
      .getOrElse(
        ZIO.fail(new IllegalArgumentException(s"Scheme ${uri.getScheme} not supported"))
      )
  }
}
