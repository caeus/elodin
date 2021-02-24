package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.compile.{LiveAst0Parser, LiveAst9Parser, LiveLexer}
import io.github.caeus.felurian.value.Value
import zio.test._
import zio.{Has, ZIO}

object ReducerSpec extends DefaultRunnableSpec {
  val lexer   = new LiveLexer
  val parser0 = new LiveAst0Parser
  val parser9 = new LiveAst9Parser
  val reducer = new LiveReducer

  val moduleSystem = new LiveModuleSystem
  def codeEvalsTo[V](code: String, v: V)(implicit toValue: ToValue[ToValue.EvalIO, V]) = {
    testM(code) {
      ZIO.service[ModuleSystem].flatMap { system =>
        toValue(v)
          .flatMap { v =>
            assertM(run(code).either)(Assertion.isRight(Assertion.equalTo(v)))
          }
          .provide(system)
      }
    }

  }
  def run(code: String): ZIO[ModuleSystem, Throwable, Value] = {

    for {
      tokens   <- lexer.lex(code)
      ast0     <- parser0.parse(tokens)
      ast9     <- parser9.parse(ast0)
      bindings <- moduleSystem.moduleBindings("test")
      result   <- reducer.run(ast9, bindings)
    } yield result
  }

  def spec =
    suite("pure")(
      codeEvalsTo("2+2", BigInt(4)),
      codeEvalsTo("-3+2", BigInt(-1)),
      codeEvalsTo("4 :: 3 :: [1 , 2]", List(BigInt(4), BigInt(3), BigInt(1), BigInt(2))),
      codeEvalsTo(""" "hello " ++ "world" """, "hello world"),
      codeEvalsTo(""""abcde".charAt(1)""", "b"),
      codeEvalsTo("true.not", false),
      codeEvalsTo("""3.{v=>v}""", BigInt(3)),
      codeEvalsTo("""[1,2,3].List  #  map({v=> v+3})""", List(BigInt(4), BigInt(5), BigInt(6))),
      codeEvalsTo("""
                    |fun pepe(num) = num+5;
                    |5.pepe
                    |""".stripMargin, BigInt(10)),
      codeEvalsTo("""let x = "WTF"; x""".stripMargin, "WTF"),
      codeEvalsTo(
        """
          |fun fib(n)={
          |	n.({n=>
          |	 do assert(n<=1);
          |	 n
          |	}.orElse({
          |		n=>
          |		fib(n-1) + fib(n- 2)
          |	}))
          |};
          |fib(6)
          |""".stripMargin,
        BigInt(8)
      ),
      codeEvalsTo("""true.assert""", true),
      codeEvalsTo("""false.assert""", false) @@ TestAspect.failing,
      codeEvalsTo("-2+2", BigInt(0)),
      codeEvalsTo("2+2*3", BigInt(8))
    ).provideSome[Annotations](ann => Has(moduleSystem: ModuleSystem) ++ ann)
}
