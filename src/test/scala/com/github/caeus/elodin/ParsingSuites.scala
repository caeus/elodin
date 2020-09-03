package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.{Lexer, OldParser}
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
          //  _      <- zio.console.putStrLn(tokens.mkString("\n"))
        } yield tokens)(isNonEmpty)
      },
      testM("Parser") {
        val lexer  = new Lexer
        val parser = new OldParser
        val source = """
                      (do [
                      |        name <: $Eff.Console.readline
                      |    ] name)
                     """.stripMargin
        val task = for {
          tokens <- lexer.lex(source)

          node <- parser.parse(tokens)
          _    <- zio.console.putStrLn("-----------------------------")
          _    <- zio.console.putStrLn(node.toString)
          _    <- zio.console.putStrLn("-----------------------------")
        } yield node

        assertM(task)(anything)
      },
      test("Sources") {

        val original = Cursor.fromString("1")
        assert(original)(anything)
      }
    ) @@ TestAspect.ignore
  }
}
