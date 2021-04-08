package io.github.caeus.elodin.eval

import zio.test._
import zio.{ZIO, ZLayer}

object ReducerSpec extends DefaultRunnableSpec {
  val eni: ENI.Service = ENI.make
  val interpreterLayer: ZLayer[Any, Nothing, Interpreter.Box] = {
    Interpreter.barebones.toLayer
  }

  def codeEvalsTo[V](code: String, v: V)(implicit
      toValue: ToValueIO[V]
  ): ZSpec[Any, Throwable] = {
    testM(code) {
      interpreterLayer.build.use { eval =>
        eval.get.resolve(
          toValue(v)
            .flatMap { v =>
              assertM(run(code))(Assertion.equalTo(v)).provide(eval)
            }
        )
      }

    }

  }
  def run(code: String) = {
    ZIO.service[Interpreter.Service].flatMap { interpreter =>
      interpreter.run("test", code)
    }
  }

  def spec =
    suite("pure")(
      suite("ignored")(
        ) @@ TestAspect.ignore,
      codeEvalsTo("2+2", BigInt(4)),
      codeEvalsTo("-3+2", BigInt(-1)),
      codeEvalsTo("1::[]", List(BigInt(1))),
      codeEvalsTo(""" "hello " ++ "world" """, "hello world"),
      codeEvalsTo(""""abcde".charAt(1)""", "b"),
      codeEvalsTo("true.not", false),
      codeEvalsTo("""[1,2,3].List  #  map{v=> v+3}""", List(BigInt(4), BigInt(5), BigInt(6))),
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
      codeEvalsTo("""3.{v=>v}""", BigInt(3)),
      codeEvalsTo(
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
        BigInt(0)
      ) @@ TestAspect.failing
    )
}
