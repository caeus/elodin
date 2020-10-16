package io.github.caeus.elodin.archive

import io.github.caeus.elodin.core.Val.IntS
import io.github.caeus.elodin.core.{Archive, Book, Thunk, ValRef}
import io.github.caeus.elodin.{ContextElodinEval, ElodinEval}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object BookBuilderSuites extends DefaultRunnableSpec {
  case class TestKit(
      atomizer: ElodinEval,
      book: Book
  )
  import TypedArg._

  def testBook[R, E](label: String)(book: Book)(f: TestKit => ZIO[R, E, TestResult]) = {
    testM(label) {
      val archive = Archive.make(Seq(book))
      val eval    = new ContextElodinEval(archive, Nil)
      f(TestKit(eval, book))
    }
  }
  override def spec =
    suite("BookBuilder")(
      testBook("Zero arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .thunk("test1")(
            _.calculate(_ => 3)
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.thunk("test1"))
          result    <- calculate.calc(Nil).provide(kit.atomizer)
        } yield {
          assert(calculate)(
            hasField("arity", (_: Thunk).arity, equalTo(0))
          ) && assert(result) {
            isSubtype[IntS](
              hasField("value", _.value, equalTo[Any, Any](BigInt(3)))
            )
          }
        }
      },
      testBook("1 arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .thunk("test2")(
            _.at(value #: _).calculate(_ => 3)
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.thunk("test2"))
          result1   <- calculate.calc(Nil).provide(kit.atomizer).either
          result2 <- calculate
                      .calc(List(ValRef(())))
                      .provide(kit.atomizer)
                      .either
        } yield {
          assert(calculate)(
            hasField("arity", (_: Thunk).arity, equalTo(1))
          ) && assert(result1) {
            isLeft(
              anything
            )
          } && assert(result2) {
            isRight(anything)
          }
        }
      },
      testBook("2 arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .thunk("test2")(
            _.at(value #: _) calculate (_ => BigInt(3))
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.thunk("test2"))
          result1   <- calculate.calc(Nil).provide(kit.atomizer).either
          result2   <- calculate.calc(List(ValRef(()))).provide(kit.atomizer).either
        } yield {
          assert(calculate)(
            hasField("arity", (_: Thunk).arity, equalTo(1))
          ) && assert(result1) {
            isLeft(
              anything
            )
          } && assert(result2) {
            isRight(
              anything
            )
          }
        }
      }
    )
}
