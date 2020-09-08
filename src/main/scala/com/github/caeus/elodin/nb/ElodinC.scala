package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.compile.{Assembler, Lexer, Parser}
import com.github.caeus.elodin.nb.archive.{Archive, Book}
import zio.Task

trait ElodinC {
  def compile(moduleName: String, code: String): Task[Book]
}

final class DefaultElodinC(modules: Archive) extends ElodinC {
  private val lexer     = Lexer.make
  private val parser    = Parser.make
  private val assembler = Assembler.make(modules)
  override def compile(moduleName: String, code: String): Task[Book] =
    for {
      tokens <- lexer.lex(code)
      node   <- parser.parse(tokens)
      module <- assembler.assemble(moduleName, node)
    } yield module
}

object ElodinC {
  def make(deps: Seq[Book]) =
    for {
      dependencies <- Archive.make(deps)
    } yield new DefaultElodinC(dependencies)
}
