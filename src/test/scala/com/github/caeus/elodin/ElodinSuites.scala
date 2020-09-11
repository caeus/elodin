package com.github.caeus.elodin

import com.github.caeus.elodin.runtime.Workflow.Job
import com.github.caeus.elodin.runtime.{Value, Workflow}
import zio.test.Assertion._
import zio.test._

object ElodinSuites extends DefaultRunnableSpec {
  override def spec =
    suite(
      "Elodin"
    )(
      testM("predef") {
        assertM(for {
          elodinc <- ElodinC.default(Nil)
          _        = println("Died at compile?")
          book <- elodinc.compile(
                       "testM",
                       """
              |import "predef" ^{};
              |{
              |  procedure = gen.run {
              |   do
              |   println "Hola";
              |   printThis <- job("laskdj");
              |   println printThis
              |  } eff.chain
              |
              |}""".stripMargin
                     )
          runnner <- Elodin.default(List(book))
          effect <- runnner
                     .get("testM", "procedure")
          Job(wer, onSuccess, sfs) <- runnner.run(effect).map(_.asInstanceOf[Workflow.Job])
          jobFinalizationResult = Value.fromJson(""" "asd" """).getOrElse(???)
          _                    <- runnner.run(onSuccess(jobFinalizationResult))
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
              |  procedure =  gen.run { do
              |    println "What's your name?";
              |    name <- job("laksdj");
              |    println (concat ["Hello ", name, "!, how are you?"])
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
      }
    )
}
