package io.github.caeus.elodin.util

import zio.{TaskManaged, ZIO, ZManaged}

import java.net.URI
import java.nio.file.spi.FileSystemProvider
import java.nio.file.{FileSystem, FileSystems}
import java.util.Collections
import scala.jdk.CollectionConverters._
import scala.util.chaining._

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
  private def recFS(parts: List[String], currFs: FileSystem): TaskManaged[FileSystem] = {
    parts.pipe {
      case Nil => ZManaged.succeed(currFs)
      case head :: tail =>
        ZIO(zipFsProvider.newFileSystem(currFs.getPath(head), Collections.emptyMap))
          .toManaged(fs => ZIO(fs.close()).orDie)
          .flatMap { fs =>
            recFS(tail, fs)
          }
    }
  }

  def fromUri(uri: URI): TaskManaged[FileSystem] = {
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
        ZManaged.fail(new IllegalArgumentException(s"Scheme ${uri.getScheme} not supported"))
      )
  }
}
