package io.github.caeus.elodin.archive

import zio.{Task, ZIO}

sealed trait Archive {
  def realNameOf(pointer: BookPageRef): Task[String]
  def chaptersOf(book: String): Task[Set[String]]
  def bookPageOf(book: String, chapter: String): Task[BookPageRef]
  def calculationAt(pointer: BookPageRef): Task[DepCalculate]
  def actionAt(pointer: ActionRef): Task[DepPerform]
  def enrichedWith(archive: Seq[Book]): Archive
}
final class FromBooksArchive(deps: Seq[Book]) extends Archive {
  private val modules =
    deps
      .groupBy(_.name)
      .view
      .mapValues(_.head)
      .toMap

  override def bookPageOf(book: String, chapter: String): Task[BookPageRef] = {
    ZIO
      .fromOption(modules.get(book).flatMap(_.exported.get(chapter)).map { id =>
        BookPageRef(book, id)
      })
      .mapError(_ => new Exception(s""""$book".$chapter not found"""))
  }

  override def chaptersOf(book: String): Task[Set[String]] = {
    ZIO
      .fromOption(modules.get(book).map(_.exported.keySet))
      .mapError(_ => new Exception(s"module $book not found"))
  }

  override def calculationAt(page: BookPageRef): Task[DepCalculate] =
    ZIO
      .fromOption(modules.get(page.book))
      .flatMap(book =>
        ZIO
          .fromOption(book.calculation(page.page))
      )
      .mapError(_ => new Exception(s"calculation not found: ${page}"))

  override def actionAt(page: ActionRef): Task[DepPerform] =
    ZIO
      .fromOption(modules.get(page.book))
      .flatMap { book =>
        ZIO.fromOption(book.action(page.name))
      }
      .mapError(_ => new Exception(s"action not found: ${page}"))

  override def enrichedWith(archive: Seq[Book]): Archive =
    new FromBooksArchive(archive.prependedAll(modules.values))

  def realNameOf(pointer: BookPageRef): Task[String] = {
    ZIO
      .fromOption(modules.get(pointer.book).map { asd: Book =>
        asd.exported
          .find(_._2 == pointer.page)
          .map(s => "\"" ++ pointer.book ++ "\"." ++ s._1)
          .getOrElse("\"" ++ pointer.book ++ "\"[" ++ pointer.page.toString ++ "]")
      })
      .mapError(_ => new Exception(s"action not found: ${pointer}"))
  }
}
object Archive {
  def make(books: Seq[Book]): Task[Archive] = ZIO.effect(new FromBooksArchive(books))

}
