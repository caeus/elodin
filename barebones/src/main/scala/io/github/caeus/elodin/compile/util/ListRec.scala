package io.github.caeus.elodin.compile.util

import zio.{Task, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object ListRec {
  def apply(path: Path)(filter: Path => Boolean): Task[Seq[Path]] = {
    Task(
      Files
        .walk(path)
        .filter { p =>
          Files.isRegularFile(p) && filter(p)
        }
        .iterator()
        .asScala
        .toSeq
    )
  }

}
