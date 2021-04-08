package io.github.caeus.elodin.util

import zio.test.{Assertion, DefaultRunnableSpec, assert, suite}

object Pos2LocSpec extends DefaultRunnableSpec {
  def spec = {
    suite("Pos2Loc") {
      test("single") {
        val pos2Loc = Pos2Loc.fromCode("xxx\nyyy\nzzz")
        assert(pos2Loc.unsafe(0))(Assertion.equalTo(Pos2Loc.Loc(1, 0))) &&
        assert(pos2Loc.unsafe(1))(Assertion.equalTo(Pos2Loc.Loc(1, 1))) &&
        assert(pos2Loc.unsafe(2))(Assertion.equalTo(Pos2Loc.Loc(1, 2))) &&
        assert(pos2Loc.unsafe(3))(Assertion.equalTo(Pos2Loc.Loc(1, 3))) &&
        assert(pos2Loc.unsafe(4))(Assertion.equalTo(Pos2Loc.Loc(2, 0))) &&
        assert(pos2Loc.unsafe(8))(Assertion.equalTo(Pos2Loc.Loc(3, 0))) &&
        assert(pos2Loc.unsafe(9))(Assertion.equalTo(Pos2Loc.Loc(3, 1))) &&
        assert(pos2Loc.unsafe(10))(Assertion.equalTo(Pos2Loc.Loc(3, 2))) &&
        assert(pos2Loc.unsafe(11))(Assertion.equalTo(Pos2Loc.Loc(3, 3))) &&
        assert(pos2Loc.unsafe(12))(Assertion.equalTo(Pos2Loc.Loc(3, 4)))
      }
    }
  }
}
