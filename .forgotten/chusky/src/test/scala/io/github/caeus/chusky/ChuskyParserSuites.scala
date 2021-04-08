package io.github.caeus.chusky

import io.github.caeus.chusky.internal.ModuleStatement.AliasExpr
import io.github.caeus.chusky.internal.{
  DefaultChuskyLexer,
  DefaultChuskyParser,
  KindExpr,
  ModuleExpr,
  TypeExpr
}
import io.github.caeus.plutus.Cursor
import zio.test.Assertion._
import zio.test._

object ChuskyParserSuites extends DefaultRunnableSpec {
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
        lexer
          .lex("(f:*=>*)=>f(fix(f))")
          .toOption
          .flatMap(tokens => parser.funExpr.take(Cursor.fromSeq(tokens)).value)
          .get
        val moduleExpr = tokens.flatMap(tokens => parser.parse(tokens))
        assert(moduleExpr)(
          isRight(
            hasField("list", (_: ModuleExpr).foreign("list"), isSome(anything)) &&
              hasField("dict", (_: ModuleExpr).foreign("dict"), isSome(anything)) &&
              hasField("either", (_: ModuleExpr).alias("either"), isSome(anything)) &&
              hasField(
                "errorS",
                (_: ModuleExpr).alias("errorS"),
                isSome(
                  hasField(
                    "definition",
                    (_: AliasExpr).definition,
                    isSubtype[TypeExpr.ApplyExpr](
                      anything
                    )
                  )
                )
              )
          )
        )
      }
    )
}
