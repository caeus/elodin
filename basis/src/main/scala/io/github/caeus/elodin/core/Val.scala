package io.github.caeus.elodin.core

import io.github.caeus.elodin.core.Val._
import zio.{=!=, UIO}

import scala.reflect.ClassTag

sealed trait Val {

  final def to[T: FromVal]: Either[List[String], T] = FromVal[T].accept(this)
}
object Val {
  case object UnitS                               extends Val
  final case class TextS(value: String)           extends Val
  final case class IntS(value: BigInt)            extends Val
  final case class FloatS(value: BigDecimal)      extends Val
  final case class BoolS(value: Boolean)          extends Val
  final case class ListS(items: Seq[Val])         extends Val
  final case class DictS(items: Map[String, Val]) extends Val
  final case class FunS(page: ThunkRef, args: Seq[ValRef]) extends Val {
    def applyTo(seq: Seq[ValRef]): UIO[ValRef] = {
      ValRef.fromPage(page, args.appendedAll(seq))
    }
    def apply(valRefs: ValRef*): UIO[ValRef] = applyTo(valRefs)
  }
  final case class TaggedS(book: String, tag: String, value: Val) extends Val
  //final case class LiftedS(value: ValRef)                         extends Val
  //final case class ForeignS(value: Any)                           extends Val
  @inline
  def apply[T: ToVal](t: T): Val = ToVal[T].cast(t)
}

/**
  * Removed covariance as FromVal[FunS] while a subclass of `FromVal[Val]`,
  * In this very specific case, it represent more actually a `cotramap`
  * Why? `FromVal[FunS]` is an alias for `Val=>FunS`
  * FromVal[FunS]` is a little bit more like contramapping over the input Val`
  * @tparam A
  */
trait FromVal[+A] { outer =>

  def accept(value: Val): Either[List[String], A]
  final def tagged(book: String, tag: String): FromVal[A] = {
    case TaggedS(`book`, `tag`, value) => outer.accept(value)
    case p                             => Left(List(s"Tried to get a tagged($book,$tag), got a $p instead"))
  }

  final def map[B](f: A => B): FromVal[B] =
    new FromVal[B] { inner =>
      override def accept(value: Val): Either[List[String], B] = outer.accept(value).map(f)
    }

  final def orElse[B: FromVal]: FromVal[Either[A, B]] =
    (value: Val) => {
      val asA = outer.accept(value)
      if (asA.isLeft)
        FromVal[B]
          .accept(value)
          .left
          .map(asA.swap.getOrElse(Nil) ::: _)
          .map(Right.apply)
      else asA.map(Left.apply)
    }
}

object FromVal {
  @inline
  def apply[T: FromVal]: FromVal[T]     = implicitly[FromVal[T]]
  implicit val toValFromV: FromVal[Val] = value => Right(value)

  implicit val toClosureFromV: FromVal[Closure] = {
    case f: FunS => Right(Closure(f))
    case o       => Left(List(s"Tried to get a Function, got $o instead"))
  }

  /**
    * Double implicit to avoid existance of FromVal[X] when X is subclass of Val
    */
  implicit def toSubValFromV1[SubVal <: Val](implicit
      ev: SubVal =!= Val
  ): FromVal[SubVal] = ???
  implicit def toSubValFromV2[SubVal <: Val](implicit
      ev: SubVal =!= Val
  ): FromVal[SubVal] = ???

  implicit val toBigIntFromSV: FromVal[BigInt] = {
    case IntS(value) => Right(value)
    case o           => Left(List(s"Tried to get a BigInt, got $o instead"))
  }
  implicit val toBigDecimalFromSV: FromVal[BigDecimal] = {
    case FloatS(value) => Right(value)
    case o             => Left(List(s"Tried to get a BigDecimal, got $o instead"))
  }
  implicit val toStringFromSV: FromVal[String] = {
    case TextS(value) => Right(value)
    case o            => Left(List(s"Tried to get a String, got $o instead"))
  }
  implicit val toBoolFromSV: FromVal[Boolean] = {
    case BoolS(value) => Right(value)
    case o            => Left(List(s"Tried to get a String, got $o instead"))
  }
  implicit val toUnitFromSV: FromVal[Unit] = {
    case UnitS => Right(())
    case o     => Left(List(s"Tried to get Unit, got $o instead"))
  }
  implicit def toListFromSV[El: FromVal]: FromVal[List[El]] = {
    case ListS(items) =>
      val bel     = List.newBuilder[El]
      val err     = List.newBuilder[String]
      var errored = false
      items.map(FromVal[El].accept).foreach {
        case Right(value) => bel += value
        case Left(value: List[String]) =>
          errored = true
          err ++= value
      }
      if (errored) Left(err.result())
      else Right(bel.result())
    case o => Left(List(s"Tried to get List, got $o instead"))
  }
  implicit def toMapFromSV[El: FromVal]: FromVal[Map[String, El]] = {
    case DictS(items) =>
      val bel     = Map.newBuilder[String, El]
      val err     = List.newBuilder[String]
      var errored = false
      items
        .map {
          case (key, value) =>
            key -> FromVal[El].accept(value)
        }
        .foreach {
          case (key, Right(value)) => bel.addOne(key, value)
          case (_, Left(value: List[String])) =>
            errored = true
            err ++= value
        }
      if (errored) Left(err.result())
      else Right(bel.result())
    case o => Left(List(s"Tried to get a Dict, got $o instead"))
  }
}

trait ToVal[-T] { self =>
  def cast(t: T): Val
  final def tagged(book: String, tag: String): ToVal[T] = (t: T) => TaggedS(book, tag, self.cast(t))
}
object ToVal {
  @inline
  def apply[T: ToVal]: ToVal[T]                   = implicitly[ToVal[T]]
  implicit val intToVal: ToVal[Int]               = (t: Int) => Val.IntS(BigInt(t))
  implicit val longToVal: ToVal[Long]             = (t: Long) => Val.IntS(BigInt(t))
  implicit val shortToVal: ToVal[Short]           = (t: Short) => Val.IntS(BigInt(t))
  implicit val bigIntToVal: ToVal[BigInt]         = (t: BigInt) => Val.IntS(t)
  implicit val floatToVal: ToVal[Float]           = (t: Float) => Val.FloatS(BigDecimal(t))
  implicit val doubleToVal: ToVal[Double]         = (t: Double) => Val.FloatS(BigDecimal(t))
  implicit val bigDecimalToVal: ToVal[BigDecimal] = (t: BigDecimal) => Val.FloatS(t)
  implicit val stringToVal: ToVal[String]         = (t: String) => Val.TextS(t)
  implicit val boolToVal: ToVal[Boolean]          = (t: Boolean) => Val.BoolS(t)
  implicit val unitToVal: ToVal[Unit]             = _ => Val.UnitS
  implicit val valToVal: ToVal[Val]               = (t: Val) => t
  implicit val closureToVal: ToVal[Closure]       = (t: Closure) => t.fold(identity)
  implicit def seqToVal[El: ToVal]: ToVal[Seq[El]] =
    (seq: Seq[El]) => Val.ListS(seq.map(ToVal[El].cast))
  implicit def mapToVal[El: ToVal]: ToVal[Map[String, El]] =
    (map: Map[String, El]) => Val.DictS(map.view.mapValues(ToVal[El].cast).toMap)
}
