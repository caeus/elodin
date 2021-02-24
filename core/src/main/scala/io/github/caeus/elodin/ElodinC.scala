package io.github.caeus.elodin

import com.typesafe.scalalogging.LazyLogging
import io.github.caeus.elodin.compile._
import io.github.caeus.elodin.core.Archive
import zio.{IO, UIO, ZIO}

trait ElodinC {
  def compile(moduleName: String, code: String): IO[CompileError, Book]
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

  def make(archive: Archive): UIO[ElodinC] = ZIO.succeed(new DefaultElodinC(archive))

}
