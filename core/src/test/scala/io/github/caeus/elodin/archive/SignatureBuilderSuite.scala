package io.github.caeus.elodin.archive

import io.github.caeus.elodin.archive.HArgs.Zot
import io.github.caeus.elodin.runtime.{Atomizer, PathdAtomizer, Value}
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object SignatureBuilderSuite extends DefaultRunnableSpec {
  import ArgAs._

  def testSignature[R, E](label: String)(f: Atomizer => ZIO[R, E, TestResult]) = {
    testM(label) {
      Archive.make(Nil).map(new PathdAtomizer(_, Nil)).flatMap(f)
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
