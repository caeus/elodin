package io.github.caeus.elodin.compile

import boopickle.Default.Unpickle
import io.github.caeus.elodin.compile.Ast9.GuestModule
import io.github.caeus.elodin.compile.util.ListRec
import zio.{Task, ZIO}

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

trait BundleReader {
  def open(path: Path): Task[Bundle]

}

final class LiveBundleReader extends BundleReader {
  override def open(path: Path): Task[Bundle] = {
    //import boopickle.Default._
    ZIO.fail(new Exception(""))

  }
}

object LiveBundleReader {
  def make: BundleReader = new LiveBundleReader
}
