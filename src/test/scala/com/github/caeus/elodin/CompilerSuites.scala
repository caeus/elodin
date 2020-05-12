package com.github.caeus.elodin

import com.github.caeus.elodin.compiler.ModuleCompiler
import com.github.caeus.elodin.lang.Node._
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, ZSpec, _}

object CompilerSuites extends DefaultRunnableSpec {
  val node = FnNode(
    Seq("$req"),
    ApplyNode(
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
        ApplyNode(Seq(RefNode("$req"), TextNode("dict.make")))
      )
    )
  )
  val moduleCompiler = new ModuleCompiler()
  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("CompilerSuites")(
      testM("AtomExpr") {
        assertM(moduleCompiler.choosePaths(TextNode("laksjd")).map(_.toSeq))(hasSize(equalTo(1)))
      },
      testM("Undeclared ref") {
        assertM(moduleCompiler.choosePaths(RefNode("laksjd")))(anything)
      } @@ TestAspect.failure,
      testM("FnExpr") {
        assertM(moduleCompiler.choosePaths(FnNode(Seq("f"), TextNode("lakjsd"))).map(_.toSeq))(
          hasSize(equalTo(1))
        )
      }
    )
}
