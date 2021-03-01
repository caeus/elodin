package io.github.caeus.felurian.eval

import zio.test._
import zio.{Has, ZIO, ZLayer}

object ReducerSpec extends DefaultRunnableSpec {
  val eni: ENI = new LiveENI
  val evaluatorLayer: ZLayer[Any, Nothing, Has[Evaluator]] = {
    val value = ModuleRegistry.live >+> ZLayer.succeed(eni)
    value >+> Evaluator.live
  }

  def codeEvalsTo[V](code: String, v: V)(implicit
      toValue: ToValueIO[V]
  ): ZSpec[Any, Throwable] = {
    testM(code) {
      evaluatorLayer.build.use { eval =>
        eval.get.resolve(
          toValue(v)
            .flatMap { v =>
              assertM(run(code).either)(Assertion.isRight(Assertion.equalTo(v))).provide(eval)
            }
        )
      }

    }

  }
  def run(code: String) = {
    ZIO.service[Evaluator].flatMap { eval =>
      eval.compile("test", code).flatMap { x =>
        eval.module("test")
      }
    }

  }

  def spec =
    suite("pure")(
      codeEvalsTo("2+2", BigInt(4)),
      codeEvalsTo("-3+2", BigInt(-1)),
      codeEvalsTo("1::[]", List(BigInt(1))),
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
      codeEvalsTo("2+2*3", BigInt(8)),
      codeEvalsTo("1::3", BigInt(8))
    )
}
