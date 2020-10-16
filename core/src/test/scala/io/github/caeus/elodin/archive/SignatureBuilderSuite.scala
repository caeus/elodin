package io.github.caeus.elodin.archive

import io.github.caeus.elodin.core.{Archive, Val, ValRef}
import io.github.caeus.elodin.{ContextElodinEval, ElodinEval}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object SignatureBuilderSuite extends DefaultRunnableSpec {
  import TypedArg._
  def testSignature[R, E](label: String)(f: ElodinEval => ZIO[R, E, TestResult]) = {
    testM(label) {
      ZIO(Archive.make(Nil)).map(new ContextElodinEval(_, Nil)).flatMap(f)
    }
  }
  override def spec =
    suite("BookBuilder")(
      testSignature("asd") { atomizer =>
        assertM(value.coerce(ValRef(()))) {
          equalTo(Val(()))
        }.provide(atomizer)
      }
    )
}
