package com.github.caeus.plutus

import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import zio.test.Assertion._
import zio.test._

object PackerSuites extends DefaultRunnableSpec {

  def isFailed[T](
      each: Assertion[PackerError]*
  ): Assertion[PackerResult[T]] = {
    val assertErrors = each.foldLeft[Assertion[Seq[PackerError]]](exists(anything)) {
      (acum, assert) =>
        acum && exists(assert)
    }
    isSubtype[Failed](
      hasField[Failed, Seq[PackerError]](
        "errors",
        _.errors,
        assertErrors
      )
    )
  }

  def hasPackerError(
      msg: Assertion[String] = anything,
      pos: Assertion[Int] = anything
  ): Assertion[PackerError] = {
    hasField[PackerError, String]("msg", _.msg, msg) &&
    hasField[PackerError, Int](
      "pos",
      _.pos,
      pos
    )
  }
  def isDone[T](result: Assertion[T], pos: Assertion[Int]): Assertion[PackerResult[T]] = {
    isSubtype[Done[T]](
      hasField[Done[T], T]("result", _.result, result) &&
        hasField[Done[T], Int]("pos", _.pos, pos)
    )
  }

  override def spec =
    suite("PackerSuites")(
      suite("greediestOf")(),
      suite("fromIterable Packer")(
        test("succeeds") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector(1, 2, 3))))(
            isDone(equalTo(()), equalTo(3))
          ) &&
          assert(packer.take(toCursor(Vector(0, 1, 2, 3)).move(1)))(
            isDone(equalTo(()), equalTo(4))
          )
        },
        test("fails on empty cursor") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector.empty)))(
            isFailed(hasPackerError(pos = equalTo(0)))
          ) &&
          assert(packer.take(toCursor(Vector(0)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(1)))
          )
        },
        test("fails on short cursor") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector(1, 2))))(
            isFailed(hasPackerError(pos = equalTo(2)))
          ) &&
          assert(packer.take(toCursor(Vector(0, 1, 2)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(3)))
          )
        },
        test("fails on unmatching content") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector(1, 2, 4, 5))))(
            isFailed(hasPackerError(pos = equalTo(3)))
          ) && assert(packer.take(toCursor(Vector(0, 1, 2, 4, 5)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(4)))
          )
        }
      ),
      suite("fromPartialFunction Packer")(
        test("succeeds") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P[String] {
            case 1 => "Hola"
          }
          assert(packer.take(toCursor(Vector(1, 2, 3))))(
            isDone(equalTo("Hola"), equalTo(1))
          ) && assert(packer.take(toCursor(Vector(0, 1, 2, 3)).move(1)))(
            isDone(equalTo("Hola"), equalTo(2))
          )
        },
        test("Fails on empty cursor") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P[String] {
            case 1 => "Hola"
          }
          assert(packer.take(toCursor(Vector.empty)))(
            isFailed(hasPackerError(pos = equalTo(0)))
          ) && assert(packer.take(toCursor(Vector(0)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(1)))
          )
        },
        test("Fails on non empty cursor") {
          val syntax = new VectorPackerSyntax[Int]
          import syntax._
          val packer = P[String] {
            case 1 => "Hola"
          }
          assert(packer.take(toCursor(Vector(2))))(
            isFailed(hasPackerError(pos = equalTo(1)))
          ) && assert(packer.take(toCursor(Vector(0, 2)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(2)))
          )
        }
      ),
      test("succeeds packer always succeeds") {
        val syntax = new VectorPackerSyntax[Int]
        import syntax._
        assert(
          succeed("Hola").take(toCursor(Vector(1, 2, 3)).move(1))
        )(
          isDone(result = equalTo("Hola"), pos = equalTo(1))
        ) && assert(
          succeed("Hola").take(toCursor(Vector.empty))
        )(
          isDone(result = equalTo("Hola"), pos = equalTo(0))
        )
      },
      test("failed packer always fails") {
        val syntax = new VectorPackerSyntax[Int]
        import syntax._
        assert(
          syntax.fail("FUCK!").take(toCursor(Vector(1, 2, 3, 4)).move(1))
        )(isFailed(hasPackerError(msg = equalTo("FUCK!"), equalTo(1)))) &&
        assert(
          syntax.fail("FUCK!").take(toCursor(Vector.empty))
        )(isFailed(hasPackerError(msg = equalTo("FUCK!"), equalTo(0))))
      }
    )
}
