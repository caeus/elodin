package io.github.caeus.elodin.util

import io.github.caeus.elodin.compile.util.{SepEl, SepNel}
import zio.test._
import zio.test.Assertion._


object SepNelSpec extends ZIOSpecDefault {
  def cprint(any: Any) = println(pprint.tokenize(any, indent = 2, width = 10).mkString(""))

  override def spec =
    suite("SepNel")(
      test("Anything separator") {
        assert(SepNel("A", List(SepEl(1, "B"), SepEl(2, "C"))).splitWhere(_ => true)) {
          equalTo(List("A", "B", "C").map(head => SepNel[String, Int](head, Nil)))
        } &&
          assert(SepNel("A", List(SepEl(1, "B"), SepEl(2, "C"))).split(1)) {
            equalTo(List(SepNel("A", Nil), SepNel("B", List(SepEl(2, "C")))))
          } &&
          assert(SepNel("A", List(SepEl(1, "B"), SepEl(2, "C"))).split(2)) {
            equalTo(List(SepNel("A", List(SepEl(1, "B"))), SepNel("C", Nil)))
          }
      }
    )
}
