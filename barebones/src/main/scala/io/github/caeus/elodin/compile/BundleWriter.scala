package io.github.caeus.elodin.compile

import io.github.caeus.elodin.util.DeepFS
import zio._

import java.io.FileOutputStream
import java.nio.file.{FileSystem, Files, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._

object BundleWriter {
  type Box = Has[Service]
  trait Service {

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

  final class LiveService extends Service {
    private def ensureFolder(path: Path): Task[Unit] = {
      ZIO(Files.createDirectories(path)).unit
    }
    override def save(where: Path, what: Bundle): Task[Unit] = {
      import boopickle.Default._
      for {
        parentFolder <- ZIO(Option(where.toAbsolutePath.getParent))
                         .someOrFail(new IllegalArgumentException("Path doesn't have a parent"))

        bytes <- ZIO(Pickle.intoBytes(what))
        _     <- ensureFolder(parentFolder)
        _ <- ZIO(
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

  def make: Service = new LiveService
  val live          = ZLayer.succeed(make)

}
