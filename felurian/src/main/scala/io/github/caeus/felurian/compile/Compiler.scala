package io.github.caeus.felurian.compile

import zio.Task

trait Compiler {
  def compile(name: String, code: String): Task[CompiledModule]
}
final class LiveCompiler extends Compiler {
  private val lexer      = new LiveLexer
  private val ast0Parser = new LiveAst0Parser
  private val ast9Parser = new LiveAst9Parser

  override def compile(name: String, code: String): Task[CompiledModule] = {
    for {
      tokens <- lexer.lex(code)
      ast0   <- ast0Parser.parse(tokens)
      ast9   <- ast9Parser.parse(ast0)
    } yield CompiledModule.make(name, ast9)
  }
}
