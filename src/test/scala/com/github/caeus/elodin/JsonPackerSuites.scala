package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.asd.{JSON, JsonPacker}
import com.github.caeus.plutus.Cursor
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import zio.test.Assertion._
import zio.test._

object JsonPackerSuites extends DefaultRunnableSpec {

  override def spec = {
    suite("JsonPackerSuites")(
      test("JsonPacker") {
        val packer = new JsonPacker
        val value  = packer.finalPacker.take(Cursor.fromString("""
		{
			"name": "Alejandro",
			"age": 30
		}
"""))
        println(value match {
          case f @ Failed(errors) => println(f.report("""
		{
			"name": "Alejandro",
			"age": 30
		}
"""))
          case Done(asd: JSON, _)         => println(asd)
        })
        assert(value)(isSubtype[Done[_]](anything))
      }
    )
  }
}
