package io.github.caeus.elodin.util

import scala.collection.immutable.TreeSet

final case class Pos2Loc(underlying: TreeSet[(Int, Int)]) {
  def unsafe(pos: Int): Pos2Loc.Loc = {
    val (offset, line) = underlying.rangeTo(pos -> Int.MaxValue).last
    Pos2Loc.Loc(line, pos - offset)
  }

  def linePositions: Seq[Int] =underlying.toSeq.map(_._1)
}

object Pos2Loc {
  final case class Loc(line: Int, col: Int)
  def fromCode(code: String): Pos2Loc = {
    val builder = TreeSet.newBuilder[(Int, Int)]
    var line    = 1
    builder += (0 -> line)
    for (pos <- 0 until code.length) {
      if (code.charAt(pos) == '\n') {
        line += 1
        builder += ((pos + 1) -> line)
      }
    }
    Pos2Loc(builder.result())
  }
}
