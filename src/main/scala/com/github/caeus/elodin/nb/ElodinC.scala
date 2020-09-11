package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.compile.{Assembler, Lexer, Parser}
import com.github.caeus.elodin.nb.archive.{Archive, Book, PredefArchive}
import com.typesafe.scalalogging.LazyLogging
import zio.{Task, UIO, ZIO}

trait ElodinC {
  def compile(moduleName: String, code: String): Task[Book]
}

final class DefaultElodinC(modules: Archive) extends ElodinC with LazyLogging {
  private val lexer     = Lexer.make
  private val parser    = Parser.make
  private val assembler = Assembler.make(modules)
  override def compile(bookName: String, code: String) = {
    logger.info(s"Compilation for book $bookName")
    for {
      tokens <- lexer.lex(code)
      node   <- parser.parse(tokens)
      book   <- assembler.assemble(bookName, node)
    } yield {
      book
    }
  }
}

object ElodinC {
  def make(deps: Seq[Book]): ZIO[Any, Throwable, DefaultElodinC] =
    for {
      dependencies <- Archive.make(deps)
    } yield new DefaultElodinC(dependencies)

  def make(archive: Archive): UIO[ElodinC] = ZIO.succeed(new DefaultElodinC(archive))

  def default(extra: Seq[Book]) =
    PredefArchive.archiveM
      .map(_.enrichedWith(extra))
      .flatMap(make)
}
