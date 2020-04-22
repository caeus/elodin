package com.github.caeus.elodin

import com.github.caeus.elodin.frontend.asd.{JSON, JsonPacker}
import com.github.caeus.plutus.Cursor
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.Slicer.StringSlicer
import zio.test.Assertion._
import zio.test._

object JsonPackerSuites extends DefaultRunnableSpec {

  override def spec = {
    suite("JsonPackerSuites")(
      test("JsonPacker") {
        val packer = new JsonPacker
        val value = packer.run(
          """
	 	          {
	           		"name": "Alejandro",
	           		"age": 30
           		}
               """)
        value.left.foreach { ex =>
          println(ex.getMessage)
        }

        assert(value)(isSubtype[Right[_, _]](anything))
      }
    )
  }
}
