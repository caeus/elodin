package io.github.caeus.elodin.runtime

import io.github.caeus.elodin.ContextElodinEval
import io.github.caeus.elodin.archive.BookBuilder
import io.github.caeus.elodin.archive.HArgs._
import io.github.caeus.elodin.core.{Archive, ThunkRef, Val, ValRef}
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}
import zio.{Has, ZIO, ZLayer}

object ElodinEvalSuites extends DefaultRunnableSpec {
  val testBook = {
    import io.github.caeus.elodin.archive.TypedArg._
    BookBuilder
      .withTitle("test_book")
      .thunk("sum")(
        _.at(is[BigInt] #: is[BigInt] #: _)
          .calculate {
            case s1 #: s2 #: _ => s1 + s2
          }
      )
      .build
  }
  val archiveLayer: ZLayer[Any, Nothing, Has[Archive]] =
    ZIO.succeed(Archive.make(Seq(testBook))).toLayer
  override def spec = {
    suite("(Context)ElodinEval")(
      testM("application") {
        for {
          archive <- ZIO.service[Archive]
          eval     = new ContextElodinEval(archive, Nil)
          _1       = ValRef(1)
          _2       = ValRef(2)
          sum <- eval
                  .get(ThunkRef("test_book", "sum"))
                  .flatMap(_.memoEval(eval))
                  .map(_.asInstanceOf[Val.FunS])
          result <- sum(_1, _2).flatMap(ref => ref.memoEval(eval))
        } yield assert(result)(
          isSubtype[Val.IntS](
            hasField("value", _.value, equalTo[Any, Any](BigInt(3)))
          )
        )
      }
    ).provideLayerShared(archiveLayer)
  }
}
