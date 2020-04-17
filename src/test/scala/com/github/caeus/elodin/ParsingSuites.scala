package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.{Lexer, Parser}
import com.github.caeus.elodin.interpreter.printers.ForNode
import com.github.caeus.plutus.Cursor
import zio.test.Assertion._
import zio.test._

object ParsingSuites extends DefaultRunnableSpec {

  override def spec = {
    suite("ParsingSuites")(
      testM("Lexer") {
        val lexer = new Lexer
        val task  = lexer.lex("""
            |(fn [f]
            |  (let {
            |    x : (f 04234 0.13123e+3 true false "a\\sd")
            |  } x)
            |)
          """.stripMargin)

        assertM(for {
          tokens <- task
          _      <- zio.console.putStrLn(tokens.mkString("\n"))
        } yield tokens)(isNonEmpty)
      },
      testM("Parser") {
        val lexer  = new Lexer
        val parser = new Parser
        val source = """
                       |(fn [f]
                       |  (let {
                       |    x : (f 3.4 true false 3 "as\\d")
                       |    y : {hola : "Hola" list : [1 2 3 4]}
                       |    doN : (do [
                       |        qwe <: (println "What's your name?")
                       |        name <: readline
                       |        qwe <: (println (concat ["Hello, " name "!"]))
                       |    ] ())
                       |  } x)
                       |)
                     """.stripMargin
        val task = for {
          tokens <- lexer.lex(source)
          node   <- parser.parse(tokens)
          _ = println(ForNode.print(0)(node))
        } yield node

        assertM(task)(anything)

      },
      test("Sources") {

        val original = Cursor.fromString("1")
        assert(original)(anything)
      }
    )
  }
}
