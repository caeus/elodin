package io.github.caeus.plutus

import io.github.caeus.plutus.PackerResult.{Done, Failed}

case class Window[Src, El](source: Src, from: Int, until: Int) {

  def value(implicit slicer: Slicer[Src]): Src = slicer.slice(source)(from, until)

  def sample(implicit slicer: Slicer[Src]): String =
    s"```${slicer.slice(source)(Math.max(0, until - 1), Math.min(until + 4, slicer.length(source))).toString}```"
}

sealed trait Cursor[Src, El] {
  final def head: El = elem(pos)
  def at(newPos: Int): Cursor[Src, El]
  def pos: Int
  def elem(pos: Int): El
  def move(offset: Int): Cursor[Src, El]
  def source: Src
  def isEmpty: Boolean
  def sample: String
  def iterable: Iterable[El]
  final def tail: Cursor[Src, El] = move(1)
  def length: Int
  final def failed[T](msg: String, cursor: Cursor[Src, El]): PackerResult[T] = {
    failed(Seq(PackerError(Nil, msg, cursor.pos)))
  }
  final def failed[T](errs: Seq[PackerError]): PackerResult[T] = {
    Failed(errs)
  }
  final def done[T](value: T, cursor: Cursor[Src, El]): PackerResult[T] =
    Done(value, cursor.pos)
}

object Cursor {

  def fromString(value: String): Cursor[String, Char] = new StringCursor(value, 0)
  def fromSeq[T](value: Seq[T]): Cursor[Vector[T], T] =
    new VectorCursor[T](value.toVector, 0)

  private abstract class AbstractCursor[O, T](pos: Int) extends Cursor[O, T] {
    @inline
    protected def ifBounded[X](pos: Int, inclusive: Boolean = false)(f: Int => X): Option[X] = {
      val range = if (inclusive) 0 to length else 0 until length
      if (range.contains(pos)) Some(f(pos))
      else None
    }
  }
  private final class StringCursor(val source: String, val pos: Int)
      extends AbstractCursor[String, Char](pos) {

    require((0 to length).contains(pos))

    override def move(offset: Int): Cursor[String, Char] =
      new StringCursor(source, pos + offset)

    override lazy val sample: String =
      if (isEmpty) "<empty cursor>" else s"```${source.slice(pos, pos + 5)}...```"

    override lazy val isEmpty: Boolean = pos == length

    override def elem(pos: Int): Char = source.charAt(pos)

    override lazy val iterable: Iterable[Char] = source.drop(pos)

    override lazy val length: Int = source.length

    override def at(newPos: Int): Cursor[String, Char] = new StringCursor(source, newPos)

  }
  private final class VectorCursor[T](val source: Vector[T], val pos: Int)
      extends AbstractCursor[Vector[T], T](pos) {

    require((0 to length).contains(pos))

    override def move(offset: Int): Cursor[Vector[T], T] =
      new VectorCursor(source, pos + offset)

    override lazy val sample: String =
      if (isEmpty) "<empty cursor>"
      else s"```${source.slice(pos, pos + 5).mkString(",")}...```"

    override lazy val isEmpty: Boolean = pos == length

    override def elem(pos: Int): T = source(pos)

    override lazy val iterable: Iterable[T] = source.view.slice(pos, source.length)

    override lazy val length: Int = source.length

    override def at(newPos: Int): Cursor[Vector[T], T] = new VectorCursor[T](source, newPos)
  }
}
object UnfinishedCursor {
  def unapply[Src, El](source: Cursor[Src, El]): Option[(El, Cursor[Src, El])] = {
    if (source.isEmpty) None
    else Some(source.head -> source.tail)
  }
}
trait ToCursor[Src, El] {
  def apply(src: Src): Cursor[Src, El]
}
object ToCursor {
  object StringToCursor extends ToCursor[String, Char] {
    override def apply(src: String): Cursor[String, Char] = Cursor.fromString(src)
  }
  object VectorToCursor extends ToCursor[Vector[Any], Any] {
    override def apply(src: Vector[Any]): Cursor[Vector[Any], Any] = Cursor.fromSeq(src)
  }
}

trait Slicer[Src] {
  def slice(src: Src)(from: Int, until: Int): Src

  def length(src: Src): Int
}

object Slicer {
  def apply[Src: Slicer]: Slicer[Src] = implicitly[Slicer[Src]]

  object StringSlicer extends Slicer[String] {
    override def slice(src: String)(from: Int, until: Int): String = src.slice(from, until)

    override def length(src: String): Int = src.length
  }

  object VectorSlicer extends Slicer[Vector[_]] {
    override def slice(src: Vector[_])(from: Int, until: Int): Vector[_] = src.slice(from, until)

    override def length(src: Vector[_]): Int = src.length
  }

}
