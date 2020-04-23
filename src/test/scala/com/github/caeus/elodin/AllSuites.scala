package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.{Lexer, Parser}
import com.github.caeus.elodin.interpreter._
import com.github.caeus.elodin.interpreter.printers.ForNode
import com.github.caeus.elodin.lang.Node._
import zio.test.Assertion._
import zio.test._
import zio.{RIO, Task}
object AllSuites extends DefaultRunnableSpec {
  val node = ApplyNode(
    Seq(FnNode(params = Seq("f"),
               LetNode(bindings = Map(
                         "x" -> ApplyNode(
                           Seq(RefNode("f"), RefNode("x"))
                         )),
                       RefNode("x"))),
        ReqNode("elodin.control/if")))

  val interpreterTest = testM("whatever") {
    val interpreter = new Interpreter(new ModuleLoader {
      override def get(name: String): Task[Val] = {
        if (name == "elodin.control/if") {
          Task.succeed(Val.Lazy(Left(Val.Native("elodin.control/if", Nil))))
        } else Task.fail(new Exception("not defined"))
      }

      override def nativeImpl(name: String): Task[NativeImpl] =
        if (name == "elodin.control/if") {
          Task.succeed(
            NativeImpl(
              3,
              reducer = {
                case Seq(cond: Val, ifTrue, ifFalse) =>
                  for {
                    interpreter: Interpreter <- RIO.environment[Interpreter]
                    cond                     <- interpreter.reduce(cond)
                    result <- cond match {
                      case Val.Bool(true)  => Task.succeed(ifTrue)
                      case Val.Bool(false) => Task.succeed(ifFalse)
                      case x =>
                        Task.fail(
                          new Exception(
                            s"Expected a boolean but got a ${x.getClass.getSimpleName}"))
                    }
                  } yield result
              }
            ))
        } else {
          ???
        }
    })

    val lexer  = new Lexer
    val parser = new Parser
    val source = """
                   $Eff.readln
                 """.stripMargin
    for {
      tokens <- lexer.lex(source)
      _ = println(tokens)
      node   <- parser.parse(tokens)
      _ = println(ForNode.print(node))
      r <- interpreter.run("lakjsdklasd", node)
      _ <- zio.console.putStrLn(r.toString)
    } yield assert(true)(isTrue)

  }
  override def spec = {

    suite("AllSuites")(
      interpreterTest
    )@@ TestAspect.ignore
  }
}
