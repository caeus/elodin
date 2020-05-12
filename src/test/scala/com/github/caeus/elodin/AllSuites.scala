package com.github.caeus.elodin

import com.github.caeus.elodin.lang.Node._
import zio.test._
object AllSuites extends DefaultRunnableSpec {
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

  override def spec = {
    suite("AllSuites")(
      ) @@ TestAspect.ignore
  }
}
