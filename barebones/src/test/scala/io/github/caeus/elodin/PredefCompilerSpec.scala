package io.github.caeus.elodin

import zio.test._

object PredefCompilerSpec extends ZIOSpecDefault {
  def spec =
    suite("PredefCompiler")(
      test("alsjkd") {
        ???
      } @@ TestAspect.ignore
    )
}
