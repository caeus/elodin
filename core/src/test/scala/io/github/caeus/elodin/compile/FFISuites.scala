package io.github.caeus.elodin.compile

import java.lang.invoke.{CallSite, LambdaMetafactory, MethodHandles, MethodType}

import zio.ZIO
import zio.test._
import zio.test.environment.TestEnvironment
import zio.test.Assertion._

case class Person(name:String)
object FFISuites extends DefaultRunnableSpec { asd =>

  def print: String = "11111"

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Minimal FFI Experient")(
      test("alskjd") {
        assert("")(anything)
      }
    )
}
