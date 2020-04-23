package com.github.caeus.plutus
import com.github.caeus.plutus.PackerResult.{Done, Failed}

import scala.collection.immutable.Queue
import scala.language.implicitConversions

//I like this structure, but something is off...
/*
 * A result has only a value and tells me SOMEHOW, how the input was affected!
 * How? A result obviously holds the value, and also a transformation on the original input.
 * How?
 *
 * */

case class PackerError(path: List[String], msg: String, pos: Int)

sealed trait PackerResult[+Out] extends {
  def value: Option[Out]

  final def map[Out1](func: Out => Out1): PackerResult[Out1] =
    this match {
      case Done(result, pos) =>
        Done(func(result), pos)
      case f => f.asInstanceOf[PackerResult[Out1]]
    }
}

object PackerResult {

  case class Failed(
      errors: Seq[PackerError]
  ) extends PackerResult[Nothing] {
    override def value: Option[Nothing] = None
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

case class Packer[Src, El, +Out](run: Cursor[Src, El] => PackerResult[Out]) {
  def take(input: Cursor[Src, El]) = run(input)
}

object Packer {

  def make[Src, El, Out](take: Cursor[Src, El] => PackerResult[Out]): Packer[Src, El, Out] =
    Packer(take)

  implicit final class PackerOps[Src, El, Out](private val packer: Packer[Src, El, Out])
      extends AnyVal {

    final def flatMap[Out1](func: Out => Packer[Src, El, Out1]): Packer[Src, El, Out1] =
      make { input: Cursor[Src, El] =>
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

    final def map[Out1](func: Out => Out1): Packer[Src, El, Out1] =
      flatMap(out => Packer.succeed(func(out)))

  }

  def failed[Src, El, T](msg: String): Packer[Src, El, T] =
    make { input =>
      input.failed(msg, input)
    }

  def capture[Src, El, Out](value: Packer[Src, El, Out]): Packer[Src, El, Window[Src, El]] =
    make { input: Cursor[Src, El] =>
      value.take(input) match {
        case Done(_, pos) =>
          Done(Window(input.source, input.pos, pos), pos)
        case f => f.asInstanceOf[PackerResult[Window[Src, El]]]
      }
    }

  def fromPartial[Src, El, Out](predicate: PartialFunction[El, Out]): Packer[Src, El, Out] =
    make { input: Cursor[Src, El] =>
      UnfinishedCursor.unapply[Src, El](input) match {
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

  def repeatUpTo[Src, El, Out](
      packer: Packer[Src, El, Out],
      upTo: Option[Int],
      sep: Packer[Src, El, _]
  ): Packer[Src, El, Vector[Out]] = {
    def recursive(upTo: Option[Int], accum: Queue[Out]): Packer[Src, El, Queue[Out]] = {
      upTo.map(_.compareTo(0)).getOrElse(1) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0  => succeed(accum)
        case 1 =>
          val sepPacker: Packer[Src, El, Option[Out]] =
            greediestOf(
              succeed[Src, El, Unit](()),
              if (accum.nonEmpty)
                sep.flatMap(_ => packer)
              else
                packer
            ).map(_.toOption)
          sepPacker.flatMap {
            case None =>
              succeed(accum)
            case Some(value) =>
              recursive(upTo.map(_ - 1), accum.enqueue(value))
          }
      }
    }

    recursive(upTo, Queue.empty).map(_.toVector)
  }

  def repeatExactly[Src, El, Out](
      packer: Packer[Src, El, Out],
      exactly: Int,
      sep: Packer[Src, El, _]
  ): Packer[Src, El, Vector[Out]] = {
    def recursive(exactly: Int, accum: Queue[Out]): Packer[Src, El, Queue[Out]] = {
      exactly.compareTo(0) match {
        case -1 => throw new IllegalArgumentException("Repeat quantity must be greater than 0")
        case 0  => succeed(accum)
        case 1 =>
          val sepPacker = if (accum.isEmpty) packer else sep.flatMap(_ => packer)
          sepPacker.flatMap { value =>
            recursive(exactly - 1, accum.enqueue(value))
          }
      }
    }

    recursive(exactly, Queue.empty).map(_.toVector)
  }

  private def isPrefix[T](that: Iterable[T], of: Iterable[T]): (Int, Boolean) = {
    val thatI = that.iterator
    val ofI   = of.iterator
    var taken = 0
    while (true) {
      if (!thatI.hasNext)
        return taken -> true
      if (!ofI.hasNext)
        return taken -> false
      taken = taken + 1
      if (thatI.next() != ofI.next()) {
        return taken -> false
      }

    }
    null
  }

  /**
    * This, mathematically speaking, is the `pure` of the packer monad `Monad[Packer[Src,El,?]]`
    *
    * @param value
    * @tparam Src
    * @tparam El
    * @tparam Out
    * @return
    */
  def succeed[Src, El, Out](value: Out): Packer[Src, El, Out] =
    make { input =>
      input.done(value, input)
    }

  def fromIterable[Src, El](from: Iterable[El]): Packer[Src, El, Unit] =
    make { input: Cursor[Src, El] =>
      isPrefix(from, input.iterable) match {
        case (taken, true) =>
          input.done((), input.move(taken))
        case (taken, false) =>
          input.failed(
            s"'${input.sample}' didn't match '${from.mkString("")}'",
            input.move(taken)
          )
      }
    }

  def greediestOf[Src, El, Out1, Out2](
      packer1: Packer[Src, El, Out1],
      packer2: Packer[Src, El, Out2]
  ): Packer[Src, El, Either[Out1, Out2]] =
    make { input =>
      val value = (packer1.take(input), packer2.take(input))
      value match {
        case (r1 @ Done(_, pos1), r2 @ Done(_, pos2)) =>
          if (pos1 < pos2)
            r2.map(Right.apply)
          else r1.map(Left.apply)
        case (r @ Done(_, _), _) => r.map(Left.apply)
        case (_, r @ Done(_, _)) => r.map(Right.apply)
        case (Failed(error1), Failed(error2)) =>
          input.failed(error1.appendedAll(error2))
      }
    }

  def end[Src, El]: Packer[Src, El, Unit] =
    make {
      case c @ UnfinishedCursor(_, next) =>
        c.failed(s"EOI expected but got ${c.sample} ", next): PackerResult[Unit]
      case c => c.done((), c)
    }

}
