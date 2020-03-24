package com.github.caeus.elodin

import com.github.caeus.elodin.interpreter.{Interpreter, ModuleLoader, Val}
import com.github.caeus.elodin.lang.Node._
import zio._

object Main extends App {

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val node =
      AppNode(
        Seq(
          LambdaNode(params = Seq("f"),
                     LetNode(bindings = Map("x" -> AppNode(
                               Seq(RefNode("f"), RefNode("x"))
                             )),
                             RefNode("x"))),StrNode("alksjd")))
    val interpreter = new Interpreter(new ModuleLoader {
      override def get(name: String): Task[Val] = ???

      override def nativeArity(name: String): Task[(Int, Seq[Val] => Task[Val])] = ???
    })

    interpreter
      .run("alksdjasd", node)
      .map { x =>
        println(x)
        x
      }
      .fold(_ => 1, _ => 0)

  }
}
