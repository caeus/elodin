package com.github.caeus.plutus
import scala.language.implicitConversions

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

case class PackerError(path: List[String], msg: String, pos: Int)

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

object Packer extends StrictLogging {

  def make[Src, El, Out](take: Cursor[Src, El] => PackerResult[Out]) =
    Packer(take)

  def failed[Col, El, T](msg: String): Packer[Col, El, T] =
    make { input =>
      input.failed(msg, input)
    }

  @inline
  private implicit def toPackerOps[Src, El, Out](
      packer: Packer[Src, El, Out]): PackerOps[Src, El, Out] = new PackerOps[Src, El, Out](packer)

  class PackerOps[Src, El, Out](private val packer: Packer[Src, El, Out]) extends AnyVal {

    def ~[Out1, R](next: => Packer[Src, El, Out1])(
        implicit concat: TConcat.Aux[Out, Out1, R]): Packer[Src, El, R] =
      packer.flatMap(a => next.map(b => concat.apply(a, b)))

    def ? : Packer[Src, El, Option[Out]] =
      repeatUpTo(packer, Some(1), pure(())).map(_.headOption)

    def rep(min: Int = 0,
            max: Option[Int] = None,
            sep: Packer[Src, El, _] = pure(())): Packer[Src, El, Vector[Out]] = {
      if (min > 0)
        for {
          prefix <- repeatExactly(packer, min, sep)
          suffix <- repeatUpTo(sep.flatMap(_ => packer), max, pure(()))
        } yield prefix.appendedAll(suffix)
      else repeatUpTo(packer, max, sep)
    }

    def rep: Packer[Src, El, Vector[Out]] = {
      repeatUpTo(packer, None, pure(()))
    }

    def ! : Packer[Src, El, Window[Src, El]] = capture(packer)

    def |[NewOut >: Out](other: Packer[Src, El, NewOut]): Packer[Src, El, NewOut] = {
      greediestOf(packer, other).map(_.fold(identity, identity))
    }

    final def named(name: String): Packer[Src, El, Out] = make { input =>
      packer.take(input) match {
        case Failed(errors) =>
          Failed(errors.map { err =>
            err.copy(path = name :: err.path)
          })
        case r => r
      }
    }

    final def logging(by: String): Packer[Src, El, Out] = make { input =>
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

    final def take(input: Src)(implicit toCursor: ToCursor[Src, El]): PackerResult[Out] =
      packer.take(toCursor(input))

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
      flatMap(out => Packer.pure(func(out)))

    final def as[Out1](value: => Out1): Packer[Src, El, Out1] = map(_ => value)

    final def ignore: Packer[Src, El, Unit] = map(_ => ())
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
          val sepPacker: Packer[Col, In, Option[Out]] =
            greediestOf(pure[Col, In, Unit](()),
                        if (accum.nonEmpty)
                          sep.flatMap(_ => packer)
                        else
                          packer).map(_.toOption)
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
  private def isPrefix[T](value: Iterable[T], of: Iterable[T], resultTaken: Int): (Int, Boolean) = {
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

  def greediestOf[Col, In, Out1, Out2](
      packer1: Packer[Col, In, Out1],
      packer2: Packer[Col, In, Out2]
  ): Packer[Col, In, Either[Out1, Out2]] = make { input =>
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

  def end[Col, In]: Packer[Col, In, Unit] = make {
    case c @ UnfinishedCursor(_, next) =>
      c.failed(s"EOI expected but got ${c.sample} ", next): PackerResult[Unit]
    case c => c.done((), c)
  }

}
