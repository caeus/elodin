package io.github.caeus.elodin.runtime

import PopResult.{Complete, Incomplete}
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object EStackSuites extends DefaultRunnableSpec {
  override def spec =
    suite("EStack")(
      testM("push N and then pops N same result") {
        checkM(Gen.listOf(Gen.anyInt)) { list =>
          for {
            stack1 <- EStack
                       .make[Int](Nil)
            _  <- stack1.pushAll(list)
            l1 <- stack1.pop(list.size).map(_.els)
          } yield assert(l1)(equalTo(list))
        }
      },
      testM("init and popAll equal") {
        checkM(Gen.listOf(Gen.anyInt)) { list =>
          for {
            stack1 <- EStack
                       .make[Int](list)
            l1 <- stack1.pop(list.size).map(_.els)
          } yield assert(l1)(equalTo(list))
        }
      },
      testM("pop more than there is") {
        checkM(Gen.listOf(Gen.anyInt)) { list =>
          for {
            stack1 <- EStack
                       .make[Int](list)
            l1 <- stack1.pop(list.size + 2)
          } yield assert(l1)(
            isSubtype[Incomplete[Int]](
              hasField("els", _.els, equalTo(list))
            )
          )
        }
      },
      testM("pop more less there is") {
        checkM(Gen.listOf(Gen.anyInt).filter(_.size > 2)) { list =>
          for {
            stack1 <- EStack
                       .make[Int](list)
            l1 <- stack1.pop(list.size - 2)
          } yield assert(l1)(
            isSubtype[Complete[Int]](
              hasField("els", _.els, equalTo(list.take(list.size - 2)))
            )
          )
        }
      }
    )
}
