package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.archive.{Archive, BookBuilder, HArgs}
import com.github.caeus.elodin.archive.HArgs._
import zio.ZIO
import zio.test.{DefaultRunnableSpec, ZSpec}
import zio.test._
import zio.test.Assertion._

object AtomizerSuites extends DefaultRunnableSpec {
  val testBook = {
    import com.github.caeus.elodin.archive.ArgAs._
    BookBuilder
      .withTitle("test_book")
      .chapter("sum")(
        _.at(is[BigInt] #: is[BigInt] #: _)
          .safeAtom {
            case s1 #: s2 #: _ => s1 + s2
          }
      )
      .build
  }
  val archiveLayer = Archive.make(Seq(testBook)).toLayer.mapError(e => TestFailure.fail(e))
  override def spec = {
    suite("(Pathd)Atomizer")(
      testM("application") {
        for {
          archive <- ZIO.service[Archive]
          atomizer = new PathdAtomizer(archive, Nil)
          _1      <- ZIO.fromEither(Value.fromJson("1"))
          _2      <- ZIO.fromEither(Value.fromJson("2"))
          sum     <- atomizer.get("test_book", "sum")
          result  <- atomizer.atomize(sum(_1, _2))
        } yield assert(result)(
          isSubtype[Value.Atom](
            hasField("value", _.of, equalTo[Any,Any](BigInt(3)))
          )
        )
      }
    ).provideLayerShared(archiveLayer)
  }
}
