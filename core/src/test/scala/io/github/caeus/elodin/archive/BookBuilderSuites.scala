package io.github.caeus.elodin.archive

import io.github.caeus.elodin.{ElodinEval, CtxEval}
import io.github.caeus.elodin.archive.asd.BookBuilder
import io.github.caeus.elodin.runtime.Value
import io.github.caeus.elodin.runtime.Value.Atom
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object BookBuilderSuites extends DefaultRunnableSpec {
  import io.github.caeus.elodin.archive.asd.TypedArg._
  case class TestKit(
                      atomizer: ElodinEval,
                      book: Book
  )

  def testBook[R, E](label: String)(book: Book)(f: TestKit => ZIO[R, E, TestResult]) = {
    testM(label) {
      for {
        archive <- Archive.make(Seq(book))
        atomizer = new CtxEval(archive, Nil)
        r       <- f(TestKit(atomizer, book))
      } yield r
    }
  }
  override def spec =
    suite("BookBuilder")(
      testBook("Zero arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .chapter("test1")(
            _.safeAtom(_ => BigInt(3))
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.calculation(0))
          result    <- calculate.form(Nil).provide(kit.atomizer)
        } yield {
          assert(calculate)(
            hasField("arity", (_: DepCalculate).arity, equalTo(0))
          ) && assert(result) {
            isSubtype[Atom](
              hasField("of", _.of, equalTo[Any, Any](BigInt(3)))
            )
          }
        }
      },
      testBook("1 arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .chapter("test2")(
            _.at(value #: _) safeAtom (_ => BigInt(3))
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.calculation(0))
          result1   <- calculate.form(Nil).provide(kit.atomizer).either
          result2   <- calculate.form(List(Value.Atom(()))).provide(kit.atomizer).either
        } yield {
          assert(calculate)(
            hasField("arity", (_: DepCalculate).arity, equalTo(1))
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
      },
      testBook("2 arity Chapter")(
        BookBuilder
          .withTitle("test_book")
          .chapter("test2")(
            _.at(value #: _) safeAtom (_ => BigInt(3))
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.book.calculation(0))
          result1   <- calculate.form(Nil).provide(kit.atomizer).either
          result2   <- calculate.form(List(Value.Atom(()))).provide(kit.atomizer).either
        } yield {
          assert(calculate)(
            hasField("arity", (_: DepCalculate).arity, equalTo(1))
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
