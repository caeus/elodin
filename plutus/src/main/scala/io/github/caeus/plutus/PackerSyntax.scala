package io.github.caeus.plutus

import com.typesafe.scalalogging.StrictLogging
import io.github.caeus.plutus.Packer._
import io.github.caeus.plutus.PackerResult.{Done, Failed}
import io.github.caeus.plutus.PackerSyntax.RichPacker
import io.github.caeus.plutus.PrettyPacker.PackerException
import io.github.caeus.plutus.Slicer.{StringSlicer, VectorSlicer}
import io.github.caeus.plutus.ToCursor.{StringToCursor, VectorToCursor}

import scala.language.implicitConversions
import scala.util.Random
import scala.util.matching.Regex

trait PackerSyntax[Src, El] {

  def fromSrc(src: Src): Packer[Src, El, Unit]

  def fromEls(el: Seq[El]): Packer[Src, El, Unit]

  def fromPartial[T](partial: PartialFunction[El, T]): Packer[Src, El, T]

  final def succeed[T](value: T): Packer[Src, El, T] = Packer.succeed(value)

  final def P(src: Src): Packer[Src, El, Unit] = fromSrc(src)

  final def P(els: El*): Packer[Src, El, Unit] = fromEls(els)

  final def P(pred: El => Boolean): Packer[Src, El, El] =
    fromPartial {
      case el if pred(el) => el
    }

  final def P[T](partial: PartialFunction[El, T]): Packer[Src, El, T] =
    fromPartial(partial)

  final def End: Packer[Src, El, Unit] = Packer.End

  final def fail[T](msg: String): Packer[Src, El, T] = Packer.failed(msg)

  implicit def slicer: Slicer[Src]

  implicit def toCursor: ToCursor[Src, El]

  implicit final def toRichPacker[Out](packer: Packer[Src, El, Out]): RichPacker[Src, El, Out] =
    new RichPacker[Src, El, Out](packer)

}

object PackerSyntax extends StrictLogging {

  class WhitespaceSyntax(sep_ : Packer[String, Char, Unit]) extends PackerSyntax[String, Char] {

    private val sep = sep_.rep.as(())

    override def fromSrc(src: String): Packer[String, Char, Unit] = sep ~ Packer.fromIterable(src)

    override def fromEls(el: Seq[Char]): Packer[String, Char, Unit] =
      sep ~ Packer.fromIterable(el)

    override def fromPartial[T](partial: PartialFunction[Char, T]): Packer[String, Char, T] =
      sep ~ Packer.fromPartial(partial)

    override implicit val slicer: Slicer[String] = StringSlicer

    override implicit val toCursor: ToCursor[String, Char] = StringToCursor
  }

  object StringPackerSyntax extends PackerSyntax[String, Char] {
    override def fromSrc(src: String): Packer[String, Char, Unit] =
      Packer.fromIterable(src)

    override def fromEls(el: Seq[Char]): Packer[String, Char, Unit] =
      Packer.fromIterable(el)

    override def fromPartial[T](partial: PartialFunction[Char, T]): Packer[String, Char, T] =
      Packer.fromPartial(partial)

    final def fromRegex(regex: Regex): Packer[String, Char, String] =
      make { input =>
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

    override implicit val slicer: Slicer[String] = StringSlicer

    override implicit val toCursor: ToCursor[String, Char] = StringToCursor
  }

  class VectorPackerSyntax[El] extends PackerSyntax[Vector[El], El] {
    override def fromSrc(src: Vector[El]): Packer[Vector[El], El, Unit] =
      Packer.fromIterable(src)

    override def fromPartial[T](partial: PartialFunction[El, T]): Packer[Vector[El], El, T] =
      Packer.fromPartial(partial)

    override def fromEls(el: Seq[El]): Packer[Vector[El], El, Unit] = Packer.fromIterable(el)

    override implicit val slicer: Slicer[Vector[El]] = VectorSlicer.asInstanceOf[Slicer[Vector[El]]]

    override implicit val toCursor: ToCursor[Vector[El], El] =
      VectorToCursor.asInstanceOf[ToCursor[Vector[El], El]]
  }

  final class RichPacker[Src, El, Out](private val packer: Packer[Src, El, Out]) extends AnyVal {

    def located: Packer[Src, El, Located[Out]] = {
      make { cursor =>
        packer.take(cursor).map(out => Located(out, cursor.pos))
      }
    }

    def none: Packer[Src, El, Option[Nothing]] = {
      as(None)
    }

    def ~[Out1, R](
                    next: => Packer[Src, El, Out1]
                  )(implicit concat: TAppend.Aux[Out, Out1, R]): Packer[Src, El, R] =
      for {
        a <- packer
        b <- next
      } yield concat(a, b)

    def ? : Packer[Src, El, Option[Out]] =
      repeatUpTo(packer, Some(1), succeed(())).map(_.headOption)

    def rep(
             min: Int = 0,
             max: Option[Int] = None,
             sep: Packer[Src, El, _] = succeed(())
           ): Packer[Src, El, Vector[Out]] = {
      if (min > 0)
        for {
          prefix <- repeatExactly(packer, min, sep)
          suffix <- repeatUpTo(sep.flatMap(_ => packer), max, succeed(()))
        } yield prefix.appendedAll(suffix)
      else repeatUpTo(packer, max, sep)
    }

    def rep: Packer[Src, El, Vector[Out]] = {
      repeatUpTo(packer, None, succeed(()))
    }

    def ! : Packer[Src, El, Window[Src, El]] = capture(packer)

    def |[Out1 >: Out](other: Packer[Src, El, Out1]): Packer[Src, El, Out1] = {
      greediestOf[Src, El, Out, Out1](packer: Packer[Src, El, Out], other: Packer[Src, El, Out1])
        .map(_.fold(identity, identity))
    }

    def named(name: String): Packer[Src, El, Out] =
      make { input =>
        logger.error(
          s"""
             |parser: $name
             |pos: ${input.pos}
             |""".stripMargin)
        packer.take(input) match {
          case Failed(errors) =>
            Failed(errors.map { err =>
              err.copy(path = name :: err.path)
            })
          case r => r
        }
      }

    def logging(by: String): Packer[Src, El, Out] =
      make { input =>
        val id = Random.alphanumeric.take(5).mkString
        logger.info(s"@$by (id: $id) is about to take ${input.sample} at pos: ${input.pos}")
        packer.take(input) match {
          case d@Done(_, pos) =>
            logger.info(s"@$by (id: $id) succeeded at pos: $pos")
            d
          case f@Failed(errors) =>
            logger.error(s"@$by (id: $id) failed ${errors.map(_.msg).mkString("\n")}")
            f
        }
      }

    def take(input: Src)(implicit toCursor: ToCursor[Src, El]): PackerResult[Out] =
      packer.take(toCursor(input))

    def flatMap[Out1](func: Out => Packer[Src, El, Out1]): Packer[Src, El, Out1] =
      make { (input: Cursor[Src, El]) =>
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

    def map[Out1](func: Out => Out1): Packer[Src, El, Out1] =
      flatMap(out => Packer.succeed(func(out)))

    def as[Out1](value: => Out1): Packer[Src, El, Out1] = map(_ => value)

    def ignore: Packer[Src, El, Unit] = as(())

    def compile(implicit
                slicer: Slicer[Src],
                toCursor: ToCursor[Src, El]
               ): PrettyPacker[Src, PackerException, Out] = PrettyPacker.version1(packer)
  }

}
