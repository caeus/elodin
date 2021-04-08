package io.github.caeus.elodin

import zio.test._

object PredefCompilerSpec extends DefaultRunnableSpec {
  def spec =
    suite("PredefCompiler")(
      testM("alsjkd") {
        ???
      } @@ TestAspect.ignore
    )
}
