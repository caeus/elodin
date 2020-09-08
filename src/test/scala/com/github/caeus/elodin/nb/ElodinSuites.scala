package com.github.caeus.elodin.nb

import com.github.caeus.elodin.nb.runtime.Value
import com.github.caeus.elodin.nb.runtime.Value.Applicable
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object ElodinSuites extends DefaultRunnableSpec {
  override def spec =
    suite(
      "Elodin"
    )(testM("full happy flow") {
      assertM(for {
        elodinc <- ElodinC.make(Nil)
        myModule <- elodinc.compile(
                     "testM",
                     """
            |import "predef" ^{};
            |{
            |procedure = do
            |  println "What's your name?";
            |  name <- readline;
            |  println (concat ["Hello ", name, "!, how are you?"])
            |}""".stripMargin
                   )
        elodin <- Elodin.make(List(myModule))
        applicable <- elodin
                       .get("testM", "procedure")
                       .flatMap {
                         case a: Applicable => ZIO.succeed(a)
                         case _             => ZIO.fail(new Exception("That is not an applicable value"))
                       }
        myResult <- ZIO.fromEither(Value.fromJson(""" "Alejandro" """))
        result   <- elodin.run(applicable(myResult))
      } yield result) {
        anything
      }
    })
}
