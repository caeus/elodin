package io.github.caeus.elodin.compile

import io.github.caeus.elodin.util.DeepFS
import zio.*

import java.io.FileOutputStream
import java.nio.file.{FileSystem, Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import boopickle.Default.*

import java.nio.ByteBuffer

trait BundleWriter {

  def save(where: Path, what: Bundle): Task[Unit]

  //    final def pack(where: Path, what: CompiledModule*): Task[Unit] = {
  //      def createZipFile(path: Path): TaskManaged[FileSystem] = {
  //        for {
  //          _ <- ZIO(Files.deleteIfExists(path)).toManaged_
  //          _ <- ZIO(Files.createDirectories(path.getParent)).toManaged_
  //          zipfs <- ZIO(DeepFS.zipFsProvider.newFileSystem(path, Map("create" -> "true").asJava))
  //                    .toManaged(fs => ZIO(fs.close()).orDie)
  //        } yield zipfs
  //      }
  //      createZipFile(where).use { fs =>
  //        save(fs.getPath("/"), what: _*)
  //      }
  //    }

}

final class LiveBundleWriter extends BundleWriter {
  private def ensureFolder(path: Path): Task[Unit] = {
    ZIO.attempt(Files.createDirectories(path)).unit
  }

  override def save(where: Path, what: Bundle): Task[Unit] = {
    import boopickle.Default._
    for {
      parentFolder <- ZIO.attempt(Option(where.toAbsolutePath.getParent))
        .someOrFail(new IllegalArgumentException("Path doesn't have a parent"))

      bytes:ByteBuffer <- ZIO.fail(new Exception("")) // ZIO.attempt(Pickle.intoBytes(what))
      _ <- ensureFolder(parentFolder)
      _ <- ZIO.attempt(
        Files.write(
          where,
          bytes.array(),
          StandardOpenOption.WRITE,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.CREATE
        )
      )
    } yield ()
  }
}
object BundleWriter{
  def make = new LiveBundleWriter
}
