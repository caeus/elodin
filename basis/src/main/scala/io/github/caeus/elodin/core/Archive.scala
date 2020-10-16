package io.github.caeus.elodin.core

import zio.UIO

trait Archive {
  def membersOf(book: String): Option[Set[String]]
  def thunkAt(ref: ThunkRef): Option[Thunk]
  def enrichedWith(books: Seq[Book]): Archive
  def contains(ref: ThunkRef): Boolean
}
final class DefaultArchive(books: Map[String, Book]) extends Archive {

  override def thunkAt(ref: ThunkRef): Option[Thunk] = {
    books
      .get(ref.book)
      .flatMap { book =>
        book.thunk(ref.id)
      }
  }

  override def membersOf(book: String): Option[Set[String]] =
    books
      .get(book)
      .map(_.exported)

  override def enrichedWith(extraBooks: Seq[Book]): Archive =
    new DefaultArchive(books ++ extraBooks.map(b => b.title -> b).toMap)

  override def contains(ref: ThunkRef): Boolean =
    books.get(ref.book).exists(_.exported.contains(ref.id))
}
object Archive {
  def make(books: Seq[Book]): Archive = new DefaultArchive(books.map(b => b.title -> b).toMap)
}
