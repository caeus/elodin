package io.github.caeus.elodin.archive

import io.github.caeus.elodin.archive.HArgs.#:
import io.github.caeus.elodin.basis.{Archive, Book, Thunk}
import io.github.caeus.elodin.basis.Val.TaggedS
import io.github.caeus.elodin.discipline.{Craft, CraftBuilder, EffectFail, EffectSucceed}
import io.github.caeus.elodin.{ContextElodinEval, ElodinEval}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object CraftBuilderSuites extends DefaultRunnableSpec {
  case class TestKit(
      atomizer: ElodinEval,
      craft: Craft
  )
  import TypedArg._
  val effBook = BookBuilder
    .withTitle("eff")
    .thunk("succeed")(
      _.at(value #: _).calculate {
        case value #: _ => EffectSucceed(value)
      }
    )
    .thunk("fail")(
      _.at(value #: _).calculate {
        case value #: _ => EffectFail(value)
      }
    )
    .build

  def testCraft[R, E](label: String)(craft: Craft)(f: TestKit => ZIO[R, E, TestResult]) = {
    testM(label) {
      val archive  = Archive.make(Seq(craft.book, effBook))
      val atomizer = new ContextElodinEval(archive, Nil)
      f(TestKit(atomizer, craft))
    }
  }
  override def spec =
    suite("BookBuilder")(
      testCraft("Zero arity Chapter")(
        CraftBuilder
          .withName("test_book")
          .effect("test1")(
            _.perform { _ =>
              ZIO
                .succeed(println("lakjsd"))
                .as(3)
            }
          )
          .build
      ) { kit =>
        for {
          calculate <- ZIO.fromOption(kit.craft.book.thunk("test1"))
          result    <- calculate.calc(Nil).provide(kit.atomizer)
        } yield {
          assert(calculate)(
            hasField("arity", (_: Thunk).arity, equalTo(0))
          ) && assert(result) {
            isSubtype[TaggedS](
              anything
            )
          }
        }
      }
    )
}
