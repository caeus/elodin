package io.github.caeus.plutus

import io.github.caeus.plutus.PackerResult.{Done, Failed}
import io.github.caeus.plutus.PackerSyntax.VectorPackerSyntax
import zio.test.Assertion._
import zio.test.{TestResult, _}

object VectorPackerSuites extends DefaultRunnableSpec {

  def isFailed[T](
      each: Assertion[PackerError]*
  ): Assertion[PackerResult[T]] = {
    val assertErrors = each.foldLeft[Assertion[Seq[PackerError]]](anything) { (union, assert) =>
      union && exists(assert)
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

  implicit val vstx: VectorPackerSyntax[Int] = new VectorPackerSyntax[Int]
  import vstx._
  def checkSuccess[Src, El](
      packer: Packer[Src, El, _],
      input: Src,
      startIndex: Int,
      result: Any,
      resultIndex: Int
  )(implicit syntax: PackerSyntax[Src, El]): TestResult = {
    assert(packer.take(syntax.toCursor(input).at(startIndex)))(
      isDone(equalTo(result), equalTo(resultIndex))
    )
  }
  def checkFailure[Src, El](
      packer: Packer[Src, El, _],
      input: Src,
      at: Int,
      errorIndexes: Set[Int]
  )(implicit syntax: PackerSyntax[Src, El]): TestResult = {
    assert(packer.take(syntax.toCursor(input).at(at)))(
      isFailed(
        if (errorIndexes.nonEmpty)
          errorIndexes.map(index => hasPackerError(pos = equalTo(index))).reduce(_ && _)
        else
          anything
      )
    )
  }
  override def spec =
    suite("PackerSuites")(
      suite("flatMap")(
        test("only first succeeds") {
          val packer1 = vstx.P(Vector(1, 2, 3)).as(1)
          val packer2 = vstx.P(Vector(4, 5, 6)).as(2)
          val packer  = packer1.flatMap(val1 => packer2.map(val2 => val1 + val2))

          checkFailure[Vector[Int], Int](packer, Vector(1, 2, 3, 5, 6, 7), 0, Set(4)) &&
          checkFailure[Vector[Int], Int](packer, Vector(0, 1, 2, 3, 5, 6, 7), 1, Set(5))

        },
        test("first fails") {
          val packer1 = P(Vector(1, 2, 3)).as(1)
          val packer2 = P(Vector(4, 5, 6)).as(2)
          val packer  = packer1.flatMap(val1 => packer2.map(val2 => val1 + val2))

          checkFailure[Vector[Int], Int](packer, Vector(2, 2, 3, 5, 6, 7), 0, Set(1)) &&
          checkFailure[Vector[Int], Int](packer, Vector(0, 2, 2, 3, 5, 6, 7), 1, Set(2))

        },
        test("both succeed") {
          val packer1 = P(Vector(1, 2, 3)).as(1)
          val packer2 = P(Vector(4, 5, 6)).as(2)
          val packer  = packer1.flatMap(val1 => packer2.map(val2 => val1 + val2))
          checkSuccess(packer, Vector(1, 2, 3, 4, 5, 6), 0, 3, 6) &&
          checkSuccess(packer, Vector(0, 1, 2, 3, 4, 5, 6), 1, 3, 7)
        }
      ),
      suite("greediestOf")(
        test("succeeds first") {
          val packer1 = P(Vector(1, 2, 3)).as(1)
          val packer2 = P(Vector(2, 3, 4)).as(2)
          val packer  = packer1 | packer2
          checkSuccess(packer, Vector(1, 2, 3), 0, 1, 3) &&
          checkSuccess(packer, Vector(0, 1, 2, 3), 1, 1, 4)
        },
        test("succeeds second") {
          val packer1 = P(Vector(2, 3, 4)).as(1)
          val packer2 = P(Vector(1, 2, 3)).as(2)

          val packer = packer1 | packer2
          checkSuccess(packer, Vector(1, 2, 3), 0, 2, 3) &&
          checkSuccess(packer, Vector(0, 1, 2, 3), 1, 2, 4)
        },
        test("both succeed, equally greedy") {
          val packer1 = P(Vector(1, 2, 3)).as(1)
          val packer2 = P(Vector(1, 2, 3)).as(2)

          val packer = packer1 | packer2
          checkSuccess(packer, Vector(1, 2, 3), 0, 1, 3) &&
          checkSuccess(packer, Vector(0, 1, 2, 3), 1, 1, 4)
        },
        test("both succeed, first greediest") {
          val packer1 = P(Vector(1, 2, 3, 4)).as(1)
          val packer2 = P(Vector(1, 2, 3)).as(2)
          val packer  = packer1 | packer2
          checkSuccess(packer, Vector(1, 2, 3, 4), 0, 1, 4) &&
          checkSuccess(packer, Vector(0, 1, 2, 3, 4), 1, 1, 5)
        },
        test("both succeed, second greediest") {
          val packer1 = P(Vector(1, 2, 3)).as(1)
          val packer2 = P(Vector(1, 2, 3, 4)).as(2)
          val packer  = packer1 | packer2

          checkSuccess(packer, Vector(1, 2, 3, 4), 0, 2, 4) &&
          checkSuccess(packer, Vector(0, 1, 2, 3, 4), 1, 2, 5)

        }
      ),
      suite("fromIterable Packer")(
        test("succeeds") {
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector(1, 2, 3))))(
            isDone(equalTo(()), equalTo(3))
          ) &&
          assert(packer.take(toCursor(Vector(0, 1, 2, 3)).move(1)))(
            isDone(equalTo(()), equalTo(4))
          )
        },
        test("fails on empty cursor") {
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector.empty)))(
            isFailed(hasPackerError(pos = equalTo(0)))
          ) &&
          assert(packer.take(toCursor(Vector(0)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(1)))
          )
        },
        test("fails on short cursor") {
          val packer = P(Vector(1, 2, 3))
          assert(packer.take(toCursor(Vector(1, 2))))(
            isFailed(hasPackerError(pos = equalTo(2)))
          ) &&
          assert(packer.take(toCursor(Vector(0, 1, 2)).move(1)))(
            isFailed(hasPackerError(pos = equalTo(3)))
          )
        },
        test("fails on unmatching content") {
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
        val result = "Hola1"
        assert(
          succeed(result).take(toCursor(Vector(1, 2, 3)).move(1))
        )(
          isDone(result = equalTo(result), pos = equalTo(1))
        ) && assert(
          succeed(result).take(toCursor(Vector.empty))
        )(
          isDone(result = equalTo(result), pos = equalTo(0))
        )
      },
      test("failed packer always fails") {
        val errorMessage = "FUCK!"
        assert(
          vstx.fail(errorMessage).take(toCursor(Vector(1, 2, 3, 4)).move(1))
        )(isFailed(hasPackerError(msg = equalTo(errorMessage), equalTo(1)))) &&
        assert(
          vstx.fail(errorMessage).take(toCursor(Vector.empty))
        )(isFailed(hasPackerError(msg = equalTo(errorMessage), equalTo(0))))
      }
    )
}
