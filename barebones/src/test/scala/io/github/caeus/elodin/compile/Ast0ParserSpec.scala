package io.github.caeus.elodin.compile

import com.typesafe.scalalogging.StrictLogging
import io.github.caeus.elodin.compile.Ast0.Expr.{EsonExpr, RefExpr}
import io.github.caeus.elodin.value.Eson.{BoolVal, TextVal}
import io.github.caeus.plutus.PrettyPacker
import zio.test._
import zio.{IO, URIO}

object Ast0ParserSpec extends DefaultRunnableSpec with StrictLogging {
  val lexer  = new LiveLexer
  val parser = new LiveAst0Parser

  def parse[X](text: String, pr: parser.Parser[X]): URIO[zio.ZEnv, Either[Object, X]] = {
    import parser.syntax._
    lexer
      .lex(text)
      .flatMap { r =>
        IO.fromEither(PrettyPacker.version1(pr ~ End).process(r))
      }
      .either
  }

  def printing[T](assertion: Assertion[T]) = {
    Assertion.assertion[T]("println")() { x =>
      println(x)
      true
    } && assertion
  }

  def testParser[X](
      label: String,
      code: String,
      prser: parser.Parser[X],
      assertion: Assertion[Either[Any, X]]
  ) = {
    testM(label) {
      assertM(parse(code, prser))(assertion)
    }
  }
  def spec =
    suite("Ast0Parser")(
      suite("block")(
        testParser(
          "simple block",
          """
                                     |asd#we
                                     |""".stripMargin,
          parser.refExpr,
          Assertion.isRight
        ),
        testParser("simple block", """
            |let x = "text";
            |x
            |""".stripMargin, parser.blockInnards, Assertion.isRight),
        testParser(
          "complex block",
          """
                                     |let x = "text";
                                     |fun asd(a)= 3;
                                     |x
                                     |""".stripMargin,
          parser.blockInnards,
          Assertion.isRight
        ),
        testParser(
          "complex block 2",
          """
             fun fib(n)={
            |	{{{v=>v}}}.orElse{n=>p}
            |}
                     |""".stripMargin,
          parser.funStatement,
          Assertion.isRight
        )
      ),
      suite("numbers")(
        testParser("short int", "123", parser.valueExpr, Assertion.isRight),
        testParser(
          "long int",
          "123123123123123",
          parser.valueExpr,
          Assertion.isRight
        ),
        testParser(
          "short float",
          "123.1",
          parser.valueExpr,
          Assertion.isRight
        ),
        testParser(
          "long float",
          "123123123123123.123123123123123",
          parser.valueExpr,
          Assertion.isRight
        ),
        testParser(
          "negative int",
          "-123",
          parser.valueExpr,
          Assertion.isRight
        ) @@ TestAspect.failing,
        testParser(
          "negative float",
          "-123.123",
          parser.valueExpr,
          Assertion.isRight
        ) @@ TestAspect.failing
      ),
      suite("text")(
        testParser(
          "text not special",
          """"pepito grillo"""",
          parser.valueExpr.map(_.value),
          Assertion.isRight(
            Assertion.isSubtype[TextVal](
              Assertion.hasField("value", _.value, Assertion.equalTo("pepito grillo"))
            )
          )
        )
      ),
      suite("boolean")(
        testParser(
          "true",
          """true""",
          parser.valueExpr.map(_.value),
          Assertion.isRight(
            Assertion.isSubtype[BoolVal](Assertion.hasField("value", _.value, Assertion.isTrue))
          )
        ),
        testParser(
          "true",
          """false""",
          parser.valueExpr.map(_.value),
          Assertion.isRight(
            Assertion.isSubtype[BoolVal](Assertion.hasField("value", _.value, Assertion.isFalse))
          )
        )
      ),
      suite("unit")(
        testParser(
          "unit",
          """unit""",
          parser.valueExpr,
          Assertion.isRight
        )
      ),
      suite("lambda")(
        testParser(
          "delimited lambda",
          """{a,b=> a + b}""",
          parser.curlyEnclosedExpr,
          Assertion.isRight
        )
      ),
      suite("delimited")(
        testParser(
          "int",
          "123",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[EsonExpr](Assertion.anything))
        ),
        testParser(
          "float",
          "123.02",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[EsonExpr](Assertion.anything))
        ),
        testParser(
          "text",
          """"aslkdj"""",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[EsonExpr](Assertion.anything))
        ),
        testParser(
          "boolean",
          """true""",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[EsonExpr](Assertion.anything))
        ),
        testParser(
          "unit",
          """unit""",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[EsonExpr](Assertion.anything))
        ),
        testParser(
          "reference",
          "x",
          parser.delimitedExpr,
          Assertion.isRight(Assertion.isSubtype[RefExpr](Assertion.anything))
        )
      ),
      suite("select")(
        testParser(
          "simple select",
          "f.x",
          parser.withComposeExpr,
          Assertion.isRight
        ),
        testParser(
          "complex select (float)",
          "f.123.12",
          parser.withComposeExpr,
          Assertion.isRight
        ),
        testParser(
          "complex select (float,float)",
          "123.123.123.12",
          parser.withComposeExpr,
          Assertion.isRight
        ),
        testParser(
          "complex select (str)",
          """f."asjkd"""",
          parser.withComposeExpr,
          Assertion.isRight
        )
      ),
      suite("apply")(
        testParser(
          "simple apply",
          "f(x)",
          parser.withComposeExpr,
          Assertion.isRight
        ),
        //(g(f))(4)
        testParser(
          "simple select",
          "f.g(4)",
          parser.withComposeExpr,
          Assertion.isRight
        )
      ),
      suite("curly enclosed expr")(
        testParser("function", """{v=>v}""", parser.curlyEnclosedExpr, Assertion.isRight)
      ),
      suite("monad comprehension")(
        testParser(
          "simple monad comprehension",
          """
            |{ identity for /*alksdj*/
            |   let! response = fetch("google.com");
            |   let! body = response.fetch#parseBody;
            |   body
            |}
            |""".stripMargin,
          parser.curlyEnclosedExpr,
          Assertion.isRight
        )
      ),
      suite("curly enclosed")(),
      suite("inline")(
        testParser(
          "simple inline",
          """2+2""",
          parser.inlineExprOnly,
          Assertion.isRight
        ),
        testParser(
          "simple inline 2",
          """-2+2""",
          parser.inlineExprOnly,
          Assertion.isRight
        ),
        testParser("simple compose expression", """
            | abc.cde
            |""".stripMargin, parser.withComposeExpr, Assertion.isRight),
        testParser(
          "complex inline",
          """identity + f(4) + "asd" + asd.qwe.ert(4)""".stripMargin,
          parser.inlineExprOnly,
          Assertion.isRight
        ),
        testParser(
          "comments",
          """
            |/*3*/
            |let a = assert(true);
            |/*5*/
            |   let b = assert(true);
            |/*7*/
            |let c = assert(true);
            |/*9*/
            |let d = assert(false);
            |/*11*/
            |d
            |""".stripMargin,
          parser.blockInnards,
          Assertion.isRight
        )
      )
    )
}
