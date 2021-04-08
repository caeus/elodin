package io.github.caeus.elodin.compile

import boopickle.Default.Unpickle
import io.github.caeus.elodin.compile.Ast9.GuestModule
import io.github.caeus.elodin.compile.util.ListRec
import zio.{Has, Task, ZIO}

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

object BundleReader {
  def make: Service = new LiveService

  type Box = Has[Service]
  trait Service {
    def open(path: Path): Task[Bundle]

  }
  final class LiveService extends Service {
    override def open(path: Path): Task[Bundle] = {
      import boopickle.Default._

      for {
        buffer <- ZIO(Files.readAllBytes(path.toAbsolutePath)).map(ByteBuffer.wrap)
        bundle <- ZIO(Unpickle.apply[Bundle].tryFromBytes(buffer).toEither)
                   .flatMap(r => ZIO.fromEither(r))
      } yield bundle
    }
  }
}
