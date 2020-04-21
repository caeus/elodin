package com.github.caeus.plutus

import com.github.caeus.plutus.Packer._

import scala.util.matching.Regex

trait SyntaxSugar[Col, El] {

  def fromCol(col: Col): Packer[Col, El, Unit]
  def fromEls(el: Seq[El]): Packer[Col, El, Unit]
  def fromPartial[T](partial: PartialFunction[El, T]): Packer[Col, El, T]

  final def P(col: Col): Packer[Col, El, Unit] = fromCol(col)
  final def P(els: El*): Packer[Col, El, Unit] = fromEls(els.toSeq)
  final def P(pred: El => Boolean): Packer[Col, El, El] = fromPartial {
    case el if pred(el) => el
  }
  final def P[T](partial: PartialFunction[El, T]): Packer[Col, El, T] =
    fromPartial(partial)

  final def End: Packer[Col, El, Unit] = Packer.end

}

object SyntaxSugar {

  object StringSyntaxSugar extends SyntaxSugar[String, Char] {
    override def fromCol(col: String): Packer[String, Char, Unit] =
      Packer.fromIterable(col)

    override def fromEls(el: Seq[Char]): Packer[String, Char, Unit] =
      Packer.fromIterable(el)

    override def fromPartial[T](partial: PartialFunction[Char, T]): Packer[String, Char, T] =
      Packer.fromPartial(partial)

    final def fromRegex(regex: Regex): Packer[String, Char, String] = make { input =>
      val matcher = regex.pattern.matcher(input.source)
      matcher.region(input.pos, matcher.regionEnd())
      if (matcher.lookingAt()) {
        val caught = matcher.group()
        input.done(caught, input.move(caught.length))
      } else {
        input.failed(s"didn't match regex ${regex.pattern.pattern()}", input)
      }
    }
    final def P(regex: Regex): Packer[String, Char, String] = fromRegex(regex)

  }
  class VectorSyntaxSugar[El] extends SyntaxSugar[Vector[El], El] {
    override def fromCol(col: Vector[El]): Packer[Vector[El], El, Unit] =
      Packer.fromIterable(col)

    override def fromPartial[T](partial: PartialFunction[El, T]): Packer[Vector[El], El, T] =
      Packer.fromPartial(partial)

    override def fromEls(el: Seq[El]): Packer[Vector[El], El, Unit] = Packer.fromIterable(el)
  }
}

object syntax {

  implicit class PredicateOps[In](val value: In => Boolean) extends AnyVal {
    def &&(other: In => Boolean): In => Boolean = { char =>
      value(char) && other(char)
    }

    def ||(other: In => Boolean): In => Boolean = { char =>
      value(char) || other(char)
    }

    def unary_! : In => Boolean = { char =>
      !value(char)
    }
  }

  implicit class PackerOps[Col, In, Out](val value: Packer[Col, In, Out]) extends AnyVal {

    def ~[Out1, R](next: => Packer[Col, In, Out1])(
        implicit concat: TConcat.Aux[Out, Out1, R]): Packer[Col, In, R] =
      value.flatMap(a => next.map(b => concat.apply(a, b)))

    def ? : Packer[Col, In, Option[Out]] =
      repeatUpTo(value, Some(1), pure(())).map(_.headOption)

    def rep(min: Int = 0,
            max: Option[Int] = None,
            sep: Packer[Col, In, _] = pure(())): Packer[Col, In, Vector[Out]] = {
      if (min > 0)
        for {
          prefix <- repeatExactly(value, min, sep)
          suffix <- repeatUpTo(sep.flatMap(_ => value), max, pure(()))
        } yield prefix.appendedAll(suffix)
      else repeatUpTo(value, max, sep)
    }

    def rep: Packer[Col, In, Vector[Out]] = {
      repeatUpTo(value, None, pure(()))
    }

    def ! : Packer[Col, In, Window[Col, In]] = capture(value)

    def |[NewOut >: Out](other: Packer[Col, In, NewOut]): Packer[Col, In, NewOut] = {
      greediestOf(value, other).map(_.fold(identity, identity))
    }
  }

}
