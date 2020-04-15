package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.{Lexer, Parser}
import com.github.caeus.plutus.Cursor
import zio.test.Assertion._
import zio.test.{assert, _}

object ParsingSuites extends DefaultRunnableSpec {

  override def spec = {
    suite("ParsingSuites")(
      testM("Lexer") {
        val lexer = new Lexer
        val task  = lexer.lex("""
            |(fn [f]
            |  (let {
            |    x : (f x)
            |  } x)
            |)
          """.stripMargin)

        assertM(for {
          tokens <- task
          //_      <- zio.console.putStrLn(tokens.mkString("\n"))
        } yield tokens)(isNonEmpty)
      },
      testM("Parser") {
        val lexer  = new Lexer
        val parser = new Parser
        val source = """
                       |(a b c d)
                     """.stripMargin
        val task = for {
          tokens <- lexer.lex(source)
          node   <- parser.parse(tokens)
          _ = println(node)
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
