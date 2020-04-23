package com.github.caeus.plutus

import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.PackerSyntax.{StringPackerSyntax, VectorPackerSyntax}
import com.github.caeus.plutus.PrettyPacker.PackerException
import zio.test._
import zio.test.Assertion._

object PackerSuites extends DefaultRunnableSpec {
  override def spec =
    suite("PackerSuites")(
      test("parial-func") {
        val syntax = new VectorPackerSyntax[Int]
        import syntax._
        val packer = P[String] {
          case 1 => "Hola"
        }
        assert(packer.take(toCursor(Vector(1, 2, 3))))(
          isSubtype[Done[String]](
            hasField[Done[String], String]("result", _.result, equalTo("Hola"))
              && hasField("pos", _.pos, equalTo(1))
          )
        )

      },
      test("success-noop") {
        val syntax = new VectorPackerSyntax[Int]
        import syntax._
        val resultAssertion = isSubtype[Done[String]](
          hasField[Done[String], String]("result", _.result, equalTo("Hola"))
            && hasField("pos", _.pos, equalTo(0))
        )
        assert(
          succeed("Hola").take(toCursor(Vector(1, 2, 3)))
        )(
          resultAssertion
        ) && assert(
          succeed("Hola").take(toCursor(Vector.empty))
        )(
          resultAssertion
        )
      },
      test("fail-always") {
        val syntax = new VectorPackerSyntax[Int]
        import syntax._
        val errorAssertion = isSubtype[Failed](
          hasField(
            "errors",
            _.errors,
            hasSize[PackerError](equalTo(1)) &&
              hasAt[PackerError](0)(
                hasField[PackerError, String]("msg", _.msg, equalTo("FUCK!")) &&
                  hasField("pos", _.pos, equalTo(0))
              )
          )
        )
        assert(
          syntax.fail("FUCK!").take(toCursor(Vector(1, 2, 3, 4)))
        )(errorAssertion) &&
        assert(
          syntax.fail("FUCK!").take(toCursor(Vector.empty))
        )(errorAssertion)
      }
    )
}
