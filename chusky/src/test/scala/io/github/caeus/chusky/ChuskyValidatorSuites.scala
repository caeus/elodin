package io.github.caeus.chusky

import io.github.caeus.chusky.internal.ModuleStatement.AliasExpr
import io.github.caeus.chusky.internal.{
  ChuskyValidator,
  DefaultChuskyLexer,
  DefaultChuskyParser,
  LocalizedError,
  ModuleExpr,
  TypeExpr
}
import io.github.caeus.plutus.{Cursor, PrettyPacker}
import zio.test.Assertion._
import zio.test._

object ChuskyValidatorSuites extends DefaultRunnableSpec {
  def spec =
    suite("ChuskyParser")(
      test("just work") {
        val lexer  = new DefaultChuskyLexer
        val parser = new DefaultChuskyParser

        val margin =
          """
            |import "other";
            |foreign list:*=>*;
            |foreign dict:*=>*;
            |foreign error:*;
            |foreign string:*;
            |alias either =(l:*,r:*)=>union{left=l,right=r};
            |alias fix=(f:*=>*)=>f(fix(f));
            |alias errorS=either(error,string);
            |""".stripMargin
        val tokens = lexer.lex(margin)

        val errors: Either[PrettyPacker.PackerException, Seq[LocalizedError]] =
          tokens.flatMap(tokens => parser.parse(tokens)).map { expr =>
            ChuskyValidator.validate(expr)
          }
        println(errors.getOrElse(Nil).mkString("\n"))
        assert(errors)(
          isRight(isNonEmpty)
        )
      }
    )
}
