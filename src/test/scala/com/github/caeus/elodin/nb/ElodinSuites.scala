package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.archive.PredefArchive
import com.github.caeus.elodin.nb.runtime.Value
import com.github.caeus.elodin.nb.runtime.Value.Applicable
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object ElodinSuites extends DefaultRunnableSpec {
  override def spec =
    suite(
      "Elodin"
    )(
      testM("predef") {
        assertM(for {
          elodinc <- ElodinC.default(Nil)
          _        = println("Died at compile?")
          myModule <- elodinc.compile(
                       "testM",
                       """
                          |import "predef" ^{};
                          |{
                          |  procedure = gen.run {
                          |   do
                          |   println "Hola";
                          |   println "Chao"
                          |  } eff.chain
                          |
                          |}""".stripMargin
                     )
          elodin <- Elodin.default(List(myModule))
          effect <- elodin
                     .get("testM", "procedure")
          _ <- elodin.run(effect)
        } yield ())(anything)
      },
      testM("full happy flow") {
        assertM(for {
          elodinc <- ElodinC.default(Nil)
          _        = println("Died at compile?")

          myModule <- elodinc.compile(
                       "testM",
                       """
            |import "predef" ^{};
            |{
            |procedure =  gen.run {do
            |  println "What's your name?";
            |  name <- readline;
            |  println (concat ["Hello ", name, "!, how are you?"])
            |  } eff.chain
            |} """.stripMargin
                     )

          elodin <- Elodin.default(List(myModule))
          effect <- elodin
                     .get("testM", "procedure")
          result <- elodin.run(effect)
        } yield result) {
          anything
        }
      } @@ TestAspect.ignore
    )
}
