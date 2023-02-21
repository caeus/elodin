package io.github.caeus.runescript
import zio.test.*

object RuneScriptSpec extends ZIOSpecDefault {
  def spec =
    test("Works") {
      assert(eval(Scope.Root(Map()), FinalAST.Int(BigInt(10))))(
        Assertion.equalTo(
          Right(BigInt(10))
        )
      )
    }
}
