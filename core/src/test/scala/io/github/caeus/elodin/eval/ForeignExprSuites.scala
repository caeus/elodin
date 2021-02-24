package io.github.caeus.elodin.eval

import io.github.caeus.elodin.ElodinC
import zio.test._

object ForeignExprSuites extends DefaultRunnableSpec{
  override def spec: ZSpec[environment.TestEnvironment, Any] = suite("ForeignExpr")(
    testM("asd"){
    //  ElodinC.
    }
  )
}
