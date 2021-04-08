package io.github.caeus.chusky

import io.github.caeus.chusky.internal.{ChuskyToken, DefaultChuskyLexer}
import io.github.caeus.chusky.internal.ChuskyToken._
import zio.test.Assertion._
import zio.test._

//import scala.language.higherKinds

object ChuskyLexerSuites extends DefaultRunnableSpec {

  def spec =
    suite("ChuskyLexer")(
      test("just work") {
        val lexer = new DefaultChuskyLexer

        val margin =
          """
            |import "other";
            |foreign list:*=>*;
            |foreign dict:*=>*;
            |alias a;
            |alias either: *=>*=>* = (l:*,r:*)=>union{left=l,right=r}
            |""".stripMargin

        val tokens = lexer.lex(margin)
        assert(tokens)(
          isRight(
            equalTo[Vector[ChuskyToken], Vector[ChuskyToken]](
              Vector(
                Import,
                Text("other"),
                Semicolon,
                Foreign,
                Name("list"),
                Colon,
                Star,
                Arrow,
                Star,
                Semicolon,
                Foreign,
                Name("dict"),
                Colon,
                Star,
                Arrow,
                Star,
                Semicolon,
                Alias,
                Name("a"),
                Semicolon,
                Alias,
                Name("either"),
                Colon,
                Star,
                Arrow,
                Star,
                Arrow,
                Star,
                Equals,
                Parenthesis.Open,
                Name("l"),
                Colon,
                Star,
                Comma,
                Name("r"),
                Colon,
                Star,
                Parenthesis.Close,
                Arrow,
                Union,
                Curly.Open,
                Name("left"),
                Equals,
                Name("l"),
                Comma,
                Name("right"),
                Equals,
                Name("r"),
                Curly.Close
              )
            )
          )
        )
      }
    )
}
