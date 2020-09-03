package com.github.caeus.elodin

import com.github.caeus.elodin.compiler.EloScript
import com.github.caeus.elodin.runtime.{EloRuntime, Val}
import com.github.caeus.elodin.types.EloEffect
import zio.test._

object BlackBoxSuites extends DefaultRunnableSpec {

  override def spec = {
    suite("BlackBoxSuites")(
      testM("happy path") {
        assertM(
          EloRuntime
            .make
            .use { runtime =>
              for {
                _ <- runtime.register(
                      EloScript
                        .StrScript("mytest", """
                |(
                |  $mod:def "dummy" (fn [hola]
                |    ($std:println hola)
                |  )
                |)
              """.stripMargin)
                    )
                func <- runtime.asFunction("mytest:dummy")
                _    <- func.apply(Val.Atom("HOLA LUCREITHINA DEL GODOY"))
              } yield ()
            }
        )(Assertion.anything)
      } @@ TestAspect.ignore
    )
  }
}
