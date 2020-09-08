package com.github.caeus.elodin.nb.archive

import zio.{Task, ZIO}

sealed trait Archive {
  def chaptersOf(book: String): Task[Set[String]]
  def page(book: String, chapter: String): Task[CalculationRef]
  def calculationAt(pointer: CalculationRef): Task[Calculate]
  def actionAt(pointer: ActionRef): Task[Perform]
}
final class FromBooksArchive(deps: Seq[Book]) extends Archive {
  private val modules =
    (deps
      .prepended(PredefArchive.predef)
      .prepended(PredefArchive.gen))
      .prepended(PredefArchive.eff)
      .groupBy(_.name)
      .view
      .mapValues(_.head)
      .toMap

  override def page(book: String, chapter: String): Task[CalculationRef] = {
    ZIO
      .fromOption(modules.get(book).flatMap(_.exported.get(chapter)).map { id =>
        CalculationRef(book, id)
      })
      .mapError(_ => new Exception(s"$book.$chapter not found"))
  }

  override def chaptersOf(book: String): Task[Set[String]] = {
    ZIO
      .fromOption(modules.get(book).map(_.exported.keySet))
      .mapError(_ => new Exception(s"module $book not found"))
  }

  override def calculationAt(page: CalculationRef): Task[Calculate] =
    ZIO
      .fromOption(modules.get(page.book))
      .flatMap(book => ZIO.fromOption(book.calculation(page.page)))
      .mapError(_ => new Exception(s"calculation not found: ${page}"))

  override def actionAt(page: ActionRef): Task[Perform] =
    ZIO
      .fromOption(modules.get(page.book))
      .flatMap { book =>
        ZIO.fromOption(book.action(page.name))
      }
      .mapError(_ => new Exception(s"action not found: ${page}"))
}
object Archive {
  def make(books: Seq[Book]): Task[Archive] = Task(new FromBooksArchive(books))
}
