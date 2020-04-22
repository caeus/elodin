package com.github.caeus.plutus

import com.github.caeus.plutus.Packer._
import com.github.caeus.plutus.Slicer.{StringSlicer, VectorSlicer}
import com.github.caeus.plutus.ToCursor.{StringToCursor, VectorToCursor}

import scala.language.implicitConversions
import scala.util.matching.Regex

trait PackerSyntax[Src, El] {

  def fromCol(col: Src): Packer[Src, El, Unit]

  def fromEls(el: Seq[El]): Packer[Src, El, Unit]

  def fromPartial[T](partial: PartialFunction[El, T]): Packer[Src, El, T]

  final def P(col: Src): Packer[Src, El, Unit] = fromCol(col)

  final def P(els: El*): Packer[Src, El, Unit] = fromEls(els.toSeq)

  final def P(pred: El => Boolean): Packer[Src, El, El] = fromPartial {
    case el if pred(el) => el
  }

  final def P[T](partial: PartialFunction[El, T]): Packer[Src, El, T] =
    fromPartial(partial)

  final def End: Packer[Src, El, Unit] = Packer.end

  implicit def slicer: Slicer[Src]

  implicit def toCursor: ToCursor[Src, El]

  implicit final def toPackerOps[Out](packer: Packer[Src, El, Out]): PackerOps[Src, El, Out] =
    new PackerOps[Src, El, Out](packer)

}

object PackerSyntax {

  object StringPackerSyntax extends PackerSyntax[String, Char] {
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

    override implicit val slicer: Slicer[String] = StringSlicer

    override implicit val toCursor: ToCursor[String, Char] = StringToCursor
  }

  class VectorPackerSyntax[El] extends PackerSyntax[Vector[El], El] {
    override def fromCol(col: Vector[El]): Packer[Vector[El], El, Unit] =
      Packer.fromIterable(col)

    override def fromPartial[T](partial: PartialFunction[El, T]): Packer[Vector[El], El, T] =
      Packer.fromPartial(partial)

    override def fromEls(el: Seq[El]): Packer[Vector[El], El, Unit] = Packer.fromIterable(el)

    override implicit val slicer: Slicer[Vector[El]] = VectorSlicer.asInstanceOf[Slicer[Vector[El]]]

    override implicit val toCursor: ToCursor[Vector[El], El] =
      VectorToCursor.asInstanceOf[ToCursor[Vector[El], El]]
  }

}
