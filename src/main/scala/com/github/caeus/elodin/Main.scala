package com.github.caeus.elodin

import com.github.caeus.elodin.interpreter.scope.Scope.Root
import com.github.caeus.elodin.interpreter.{Interpreter, ModuleLoader, NativeImpl, Val}
import com.github.caeus.elodin.lang.Node._
import zio._

object Main extends App {





  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val node =
      ApplyNode(
        Seq(FnNode(params = Seq("f"),
                       LetNode(bindings = Map(
                                 "x" -> ApplyNode(
                                   Seq(RefNode("f"), RefNode("x"))
                                 )),
                               RefNode("x"))),
            TextNode("alksjd")))
    val interpreter = new Interpreter(new ModuleLoader {
      override def get(name: String): Task[Val] = ???

      override def nativeImpl(name: String): Task[NativeImpl] = ???
    })

    interpreter
        .run(node)
      .map { x =>
        println(x)
        x
      }
      .fold(_ => 1, _ => 0)

  }
}
