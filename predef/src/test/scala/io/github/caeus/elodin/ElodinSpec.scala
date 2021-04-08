package io.github.caeus.elodin

import zio.test._

import scala.io.Source

object ElodinSpec extends DefaultRunnableSpec {
  def spec =
    suite("Elodin")(
      test("Ideal usage") {

        assert(Source.fromResource("folder/file").mkString)(Assertion.equalTo("Generated"))
      }
    )

}
