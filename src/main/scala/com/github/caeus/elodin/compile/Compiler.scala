package com.github.caeus.elodin.compile

import com.github.caeus.elodin.interep.Swap
import com.github.caeus.elodin.lang.{Node, Path}
import zio.Task

trait Compiler[-I, +O] {
  def compile(input: I): Task[O]
}

trait NodeCompiler extends Compiler[Node, Map[Path, Swap]] {}
final class DefaultNodeCompiler extends NodeCompiler {

  override def compile(input: Node): Task[Map[Path, Swap]] = {
    val node = new RootCtxNode(input)
    for {
      nodeTable: Map[Path, Set[DeclParam]] <- AnalyzeParams(node)
      _ = println(nodeTable)
      r <- new Node2Swap("whatever", node, nodeTable)
        .run()
    } yield r

  }
}
