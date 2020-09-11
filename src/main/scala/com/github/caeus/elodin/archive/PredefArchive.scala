package com.github.caeus.elodin.archive

import com.github.caeus.elodin.ElodinC
import com.typesafe.scalalogging.LazyLogging
import zio.{Task, ZIO}

object PredefArchive extends LazyLogging {

  private def compileResource(name: String, elodinC: ElodinC) = {
    for {
      code <- Task
               .effect(
                 scala.io.Source
                   .fromResource(s"elodin/predef/$name.elodin", getClass.getClassLoader)
               )
               .toManaged(m => Task.effect(m.close()).either)
               .use(b => ZIO(b.mkString))
      book <- elodinC
               .compile(name, code)
      _ = logger.info(s"Finished compiling BOOK $name")
    } yield book
  }
  private def compilePredef = {
    for {
      elodinC <- ElodinC.make(Seq(BasisArchive.book))
      _        = logger.info("Started compiling basic books")
      books <- ZIO.collectAll(Seq("dict", "eff", "gen", "list").map { name =>
                compileResource(name, elodinC)
              })
      _           = logger.info("Finished compiling basic books")
      elodinC    <- ElodinC.make(BasisArchive.book :: books)
      predefBook <- compileResource("predef", elodinC)
      archive    <- Archive.make(predefBook :: BasisArchive.book :: books)
    } yield archive

  }

  val archiveM = compilePredef

}
