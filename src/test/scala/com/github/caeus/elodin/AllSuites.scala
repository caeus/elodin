package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.{Lexer, Parser}
import com.github.caeus.elodin.interpreter.Val.Atom
import com.github.caeus.elodin.interpreter._
import com.github.caeus.elodin.interpreter.printers.ForNode
import com.github.caeus.elodin.lang.Node._
import zio.Task
import zio.test.Assertion._
import zio.test._
object AllSuites extends DefaultRunnableSpec {
  val node = ApplyNode(
    Seq(
      FnNode(
        params = Seq("f"),
        LetNode(
          bindings = Map(
            "x" -> ApplyNode(
              Seq(RefNode("f"), RefNode("x"))
            )
          ),
          RefNode("x")
        )
      ),
      ???
    )
  )

  val interpreterTest = testM("whatever") {
    val interpreter = new Interpreter(new FFI {
      def get(name: String): Task[Val] = {
        Task.succeed(Atom(Void))
      }

      override def nativeImpl(name: String): Task[NativeImpl] =
        Task.fail(new Exception("not implemented"))

      override def get(interpreter: Interpreter)(expr: Val): Task[Val] = ???
    })

    val lexer  = new Lexer
    val parser = new Parser
    val source = """
                   $Eff.readln
                 """.stripMargin
    for {
      tokens <- lexer.lex(source)
      _       = println(tokens)
      node   <- parser.parse(tokens)
      _       = println(ForNode.print(node))
      r      <- interpreter.run(node)
      _      <- zio.console.putStrLn(r.toString)
    } yield assert(true)(isTrue)

  }
  override def spec = {
    suite("AllSuites")(
      interpreterTest
    ) @@ TestAspect.ignore
  }
}
