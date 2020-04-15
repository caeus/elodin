package com.github.caeus.plutus

import com.github.caeus.plutus.PackerResult.{Done, Failed}

//sealed trait Slicer[Col, +El] {
//  def size(col: Col): Int
//  final def take(col: Col, until: Int): Option[Col] = slice(col, 0, until)
//  final def drop(col: Col, from: Int): Option[Col]  = slice(col, from, size(col))
//  def slice(col: Col, from: Int, until: Int): Option[Col]
//  def at(pos: Int): Option[El]
//}
//object Slicer {
//  implicit object StringSlicer extends Slicer[String, Char] {
//    override def size(col: String): Int = ???
//
//    override def slice(col: String, from: Int, until: Int): Option[String] = ???
//
//    override def at(pos: Int): Option[Char] = ???
//  }
//}

case class Window[Col, El](source: Col, from: Int, until: Int) {

  def from(pos: Int): Window[Col, El]          = Window(source, pos, until)
  def value(implicit slicer: Slicer[Col]): Col = slicer.slice(source)(from, until)

  def sample(implicit slicer: Slicer[Col]): String =
    s"```${slicer.slice(source)(Math.max(0, until - 1), Math.min(until + 4, slicer.length(source))).toString}```"

}

sealed trait Cursor[Col, El] {
  final def head: El = elem(pos)
  def at(newPos: Int): Cursor[Col, El]
  def pos: Int
  def elem(pos: Int): El
  def move(offset: Int): Cursor[Col, El]
  def source: Col
  def isEmpty: Boolean
  def sample: String
  def iterable: Iterable[El]
  final def tail: Cursor[Col, El] = move(1)
  def length: Int
  final def failed[T](msg: String, cursor: Cursor[Col, El]): PackerResult[T] = {
    failed(Seq(LeafErr(Nil, msg, cursor.pos)))
  }
  final def failed[T](errs: Seq[LeafErr]): PackerResult[T] = {
    Failed(errs)
  }
  final def done[T](value: T, cursor: Cursor[Col, El]): PackerResult[T] =
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

    override lazy val sample: String = {
      if (isEmpty) "<empty window>" else s"```${source.slice(pos, pos + 5)}...```"
    }
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
      if (isEmpty) "<empty window>"
      else s"```${source.slice(pos, pos + 5).mkString(",")}...```"

    override lazy val isEmpty: Boolean = pos == length

    override def elem(pos: Int): T = source(pos)

    override lazy val iterable: Iterable[T] = source.drop(pos)

    override lazy val length: Int = source.length

    override def at(newPos: Int): Cursor[Vector[T], T] = new VectorCursor[T](source, newPos)
  }
}
object UnfinishedCursor {
  def unapply[Col, El](source: Cursor[Col, El]): Option[(El, Cursor[Col, El])] = {
    if (source.isEmpty) None
    else Some(source.head -> source.tail)
  }
}
