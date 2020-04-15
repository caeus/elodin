package com.github.caeus.plutus

import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.typesafe.scalalogging.StrictLogging
import io.circe.Json

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

//I like this structure, but something is off...
/*
 * A result has only a value and tells me SOMEHOW, how the input was affected!
 * How? A result obviously holds the value, and also a transformation on the original input.
 * How?
 *
 * */

case class LeafErr(path: List[String], msg: String, pos: Int)

sealed trait PackerResult[+Out] extends {
  def value: Option[Out]
  final def map[Out1](func: Out => Out1): PackerResult[Out1] = this match {
    case Done(result, pos) =>
      Done(func(result), pos)
    case f => f.asInstanceOf[PackerResult[Out1]]
  }
}

object PackerResult {

  case class Failed(
      errors: Seq[LeafErr]
  ) extends PackerResult[Nothing] {
    override def value: Option[Nothing] =
      None

    def report[Src: Slicer](src: Src): String = {
      Json
        .arr(
          errors
            .groupBy(_.pos)
            .toSeq
            .sortBy(- _._1)
            .map {
              case (pos, errors) =>
                Json.obj(
                  "position" -> Json.fromInt(pos),
                  "details" -> Json.arr(errors.map { err =>
                    Json.obj(
                      "path"    -> Json.fromString(err.path.mkString("/")),
                      "sample"  -> Json.fromString(Slicer[Src].slice(src)(pos, pos + 5).toString),
                      "message" -> Json.fromString(err.msg)
                    )
                  }: _*)
                )
            }: _*)
        .spaces2
    }
  }

  case class Done[+Out](
      result: Out,
      pos: Int
  ) extends PackerResult[Out] {
    override def value: Option[Out] = Some(result)
  }

  //Sorry byt later case class Cont[In, Out](next: Tarser[In, Out]) extends TarserResult[In, Out]
  //Adding this makes it very difficult to add the forking ability
}

case class Packer[Col, In, +Out](run: Cursor[Col, In] => PackerResult[Out]) {
  def take(input: Cursor[Col, In]) = run(input)
}

object Packer extends StrictLogging {

  def make[Col, In, Out](take: Cursor[Col, In] => PackerResult[Out]) =
    Packer(take)

  def failed[Col, El, T](msg: String): Packer[Col, El, T] =
    make { input =>
      input.failed(msg, input)
    }

  implicit class PackerOps[Col, In, Out](private val packer: Packer[Col, In, Out]) extends AnyVal {
    final def named(name: String): Packer[Col, In, Out] = make { input =>
      packer.take(input) match {
        case Failed(errors) =>
          Failed(errors.map { err =>
            err.copy(path = name :: err.path)
          })
        case r => r
      }
    }
    final def logging(by: String): Packer[Col, In, Out] = make { input =>
      val id = Random.nextInt()
      logger.info(s"@$by (id: $id) is about to take ${input.sample} at pos: ${input.pos}")
      packer.take(input) match {
        case d @ Done(_, pos) =>
          logger.info(s"@$by (id: $id) succeeded at pos: $pos")
          d
        case f @ Failed(errors) =>
          logger.error(s"@$by (id: $id) failed ${errors.map(_.msg).mkString("\n")}")
          f
      }
    }
    final def take(input: Col)(implicit toSource: ToSource.Aux[Col, In]): PackerResult[Out] =
      packer.take(toSource.toSource(input))

    final def flatMap[Out1](func: Out => Packer[Col, In, Out1]): Packer[Col, In, Out1] =
      make { input: Cursor[Col, In] =>
        packer.take(input) match {
          case Done(value, pos) =>
            func(value).take(input.at(pos)) match {
              case Done(newWindow, newValue) =>
                Done(newWindow, newValue)
              case f =>
                f.asInstanceOf[PackerResult[Out1]]
            }
          case f => f.asInstanceOf[PackerResult[Out1]]
        }
      }
    final def map[Out1](func: Out => Out1): Packer[Col, In, Out1] =
      flatMap(out => Packer.pure(func(out)))

    final def as[Out1](value: => Out1): Packer[Col, In, Out1] = map(_ => value)
  }

  def capture[Col, El, Out](value: Packer[Col, El, Out]): Packer[Col, El, Window[Col, El]] =
    make { input: Cursor[Col, El] =>
      value.take(input) match {
        case Done(_, pos) =>
          Done(Window(input.source, input.pos, pos), pos)
        case f => f.asInstanceOf[PackerResult[Window[Col, El]]]
      }
    }
  def fromPartial[Col, In, X](predicate: PartialFunction[In, X]): Packer[Col, In, X] =
    make { input: Cursor[Col, In] =>
      UnfinishedCursor.unapply[Col, In](input) match {
        case Some((head, _)) =>
          predicate.lift(head) match {
            case Some(x) =>
              input.done(x, input.tail)
            case None =>
              input.failed(s"failed to take ${input.sample}", input.move(1))
          }
        case None =>
          input.failed(s"Expected one token, EOI gotten instead", input.move(0))
      }
    }
  def repeatUpTo[Col, In, Out](
      packer: Packer[Col, In, Out],
      upTo: Option[Int],
      sep: Packer[Col, In, _]
  ): Packer[Col, In, Vector[Out]] = {
    def recursive(upTo: Option[Int], accum: Queue[Out]): Packer[Col, In, Queue[Out]] = {
      upTo.map(_.compareTo(0)).getOrElse(1) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0  => pure(accum)
        case 1 =>
          val sepPacker =
            longestOf[Col, In, Option[Out]](pure(None),
                                            (if (accum.nonEmpty)
                                               sep.flatMap(_ => packer)
                                             else
                                               packer).map(Some(_)))
          sepPacker.flatMap {
            case None =>
              pure(accum)
            case Some(value) =>
              recursive(upTo.map(_ - 1), accum.enqueue(value))
          }
      }
    }

    recursive(upTo, Queue.empty).map(_.toVector)
  }

  def repeatExactly[Col, In, Out](
      packer: Packer[Col, In, Out],
      exactly: Int,
      sep: Packer[Col, In, _]
  ): Packer[Col, In, Vector[Out]] = {
    def recursive(exactly: Int, accum: Queue[Out]): Packer[Col, In, Queue[Out]] = {
      exactly.compareTo(0) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0  => pure(accum)
        case 1 =>
          val sepPacker = if (accum.isEmpty) packer else sep.flatMap(_ => packer)
          sepPacker.flatMap { value =>
            recursive(exactly - 1, accum.enqueue(value))
          }
      }
    }

    recursive(exactly, Queue.empty).map(_.toVector)
  }
  @tailrec
  def isPrefix[T](value: Iterable[T], of: Iterable[T], resultTaken: Int): (Int, Boolean) = {
    if (value.isEmpty)
      resultTaken -> true
    else {
      if (of.headOption.contains(value.head)) {
        isPrefix(value.tail, of.tail, resultTaken + 1)
      } else {
        resultTaken -> false
      }
    }
  }

  def pure[Col, In, Out](value: Out): Packer[Col, In, Out] = make { input =>
    input.done(value, input)
  }
  def fromIterable[Col, In](from: Iterable[In]): Packer[Col, In, Unit] =
    make { input: Cursor[Col, In] =>
      isPrefix(from, input.iterable, 0) match {
        case (taken, true) =>
          input.done((), input.move(taken))
        case (taken, false) =>
          input.failed(s"'${input.sample}' didn't match '${from.mkString("")}'", input.move(taken))
      }
    }

  def longestOf[Col, In, Out](
      packer1: Packer[Col, In, Out],
      packer2: Packer[Col, In, Out]
  ): Packer[Col, In, Out] = make { input =>
    {
      val value = (packer1.take(input), packer2.take(input))
      value match {
        case (r1 @ Done(src1, pos1), r2 @ Done(src2, pos2)) =>
          if (pos1 < pos2)
            r2
          else r1
        case (r @ Done(_, _), _) => r
        case (_, r @ Done(_, _)) => r
        case (Failed(error1), Failed(error2)) =>
          input.failed(error1.appendedAll(error2))
      }
    }
  }

  def end[Col, In]: Packer[Col, In, Unit] = make {
    case c @ UnfinishedCursor(_, next) =>
      c.failed(s"EOI expected but got ${c.sample} ", next): PackerResult[Unit]
    case c => c.done((), c)
  }

}
trait ToSource[T] {
  type El
  def toSource(value: T): Cursor[T, El]
}
object ToSource {
  type Aux[T, El1] = ToSource[T] { type El = El1 }
  implicit object StringToSource extends ToSource[String] {
    override type El = Char
    override def toSource(value: String): Cursor[String, Char] = Cursor.fromString(value)
  }

  class VectorToSource[T] extends ToSource[Vector[T]] {
    override type El = T
    override def toSource(value: Vector[T]): Cursor[Vector[T], T] = Cursor.fromSeq(value)
  }
  implicit def vectorToSource[T]: ToSource[Vector[T]] { type El = T } = new VectorToSource[T]
}
