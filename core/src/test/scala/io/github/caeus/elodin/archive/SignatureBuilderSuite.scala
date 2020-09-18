package io.github.caeus.elodin.archive

import io.github.caeus.elodin.{ElodinEval, CtxEval}
import io.github.caeus.elodin.archive.asd.HArgs.Zot
import io.github.caeus.elodin.runtime.Value
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object SignatureBuilderSuite extends DefaultRunnableSpec {
  import io.github.caeus.elodin.archive.asd.TypedArg._

  def testSignature[R, E](label: String)(f: ElodinEval => ZIO[R, E, TestResult]) = {
    testM(label) {
      Archive.make(Nil).map(new CtxEval(_, Nil)).flatMap(f)
    }
  }
  override def spec =
    suite("BookBuilder")(
      testSignature("asd") { atomizer =>
        assertM(value.coerce(Value.Atom(()))) {
          equalTo(Value.Atom(()))
        }.provide(atomizer)
      }
    )
}
