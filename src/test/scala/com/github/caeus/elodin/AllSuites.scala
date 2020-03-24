package com.github.caeus.elodin

import com.github.caeus.elodin.interpreter.{Impl, Interpreter, ModuleLoader, Val}
import com.github.caeus.elodin.lang.Node.{AppNode, LambdaNode, LetNode, RefNode, ReqNode}
import zio.test.Assertion._
import zio.test._
import zio.{RIO, Task}
object AllSuites extends DefaultRunnableSpec {
  val node = AppNode(
    Seq(LambdaNode(params = Seq("f"),
                   LetNode(bindings = Map(
                             "x" -> AppNode(
                               Seq(RefNode("f"), RefNode("x"))
                             )),
                           RefNode("x"))),
        ReqNode("elodin.control/if")))
  override def spec = suite("AllSuites")(
    testM("whatever") {
      val interpreter = new Interpreter(new ModuleLoader {
        override def get(name: String): Task[Val] =
          if (name == "elodin.control/if") {
            Task.succeed(Val.App(Impl.Native("elodin.control/if", Nil)))
          } else Task.fail(new Exception("not defined"))

        override def nativeArity(name: String): Task[(Int, Seq[Val] => RIO[Interpreter, Val])] =
          if (name == "elodin.control/if") {
            Task.succeed(3 -> {
              case Seq(cond, ifTrue, ifFalse) =>
                for {
                  interpreter <- RIO.environment[Interpreter]
                  cond        <- interpreter.reduce(cond)
                  result <- cond match {
                    case Val.Bool(true)  => Task.succeed(ifTrue)
                    case Val.Bool(false) => Task.succeed(ifFalse)
                    case x =>
                      Task.fail(
                        new Exception(s"Expected a boolean but got a ${x.getClass.getSimpleName}"))
                  }
                } yield result
            })
          } else { ??? }
      })
      interpreter
        .run("qoiwue", node)
        .map { x =>
          println(x)
          assert(true)(isTrue)

        }

    }
  )
}
