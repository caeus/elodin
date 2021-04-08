package io.github.caeus.elodin.value

import io.github.caeus.elodin.value.Eson.foldLeftEither

import scala.annotation.tailrec

sealed trait Eson {}
object Eson {
  final case class IntVal(value: BigInt)             extends Eson
  final case class FloatVal(value: BigDecimal)       extends Eson
  final case class TextVal(value: String)            extends Eson
  final case class BoolVal(value: Boolean)           extends Eson
  final case class DictVal(value: Map[String, Eson]) extends Eson
  final case class ListVal(value: Seq[Eson])         extends Eson
  //final case class TaggedVal(module: String, tag: Seq[String], value: Eson) extends Eson
  final case object UnitVal extends Eson

  def typeOf(eson: Eson): String =
    eson match {
      case IntVal(_)   => "int"
      case FloatVal(_) => "float"
      case TextVal(_)  => "text"
      case BoolVal(_)  => "bool"
      case DictVal(_)  => "dict"
      case ListVal(_)  => "list"
      case UnitVal     => "unit"
    }

  @tailrec
  final def foldLeftEither[A, B, C](list: List[Either[List[A], B]])(result: C)(
      f: (C, B) => C
  ): Either[List[A], C] = {
    list match {
      case Nil              => Right(result)
      case Right(b) :: tail => foldLeftEither(tail)(f(result, b))(f)
      case hasFailures =>
        Left(hasFailures.collect {
          case Left(value) => value
        }.flatten)
    }

  }
}
trait FromEson[+T] {
  def apply(eson: Eson): Either[List[String], T]
}
object FromEson {
  private def strict[T](typeDesc: String)(pf: PartialFunction[Eson, T]): FromEson[T] =
    (eson: Eson) => {
      pf.lift(eson).toRight {
        List(s"Expected $typeDesc, got ${Eson.typeOf(eson)} instead")
      }
    }

  implicit def listFromEson[T](implicit iFromEson: FromEson[T]): FromEson[Seq[T]] = {
    case Eson.ListVal(v) =>
      foldLeftEither(v.map { v =>
        iFromEson(v)
      }.toList)(Vector.empty[T]) {
        case (vector, v) =>
          vector.appended(v)
      }
    case eson =>
      Left(List(s"Expected list, got ${Eson.typeOf(eson)} instead"))
  }

  implicit def dictFromEson[T](implicit iFromEson: FromEson[T]): FromEson[Map[String, T]] = {
    case Eson.DictVal(v) =>
      foldLeftEither(v.map {
        case (k, v) => iFromEson(v).map(k -> _)
      }.toList)(Map.empty[String, T]) {
        case (map, (k, v)) =>
          map.updated(k, v)
      }
    case eson =>
      Left(List(s"Expected dict, got ${Eson.typeOf(eson)} instead"))
  }

  implicit val textFromEson: FromEson[String] = strict("text") {
    case Eson.TextVal(v) => v
  }
  implicit val intFromEson: FromEson[BigInt] = strict("int") {
    case Eson.IntVal(v) => v
  }
  implicit val floatFromEson: FromEson[BigDecimal] = strict("float") {
    case Eson.FloatVal(v) => v
  }
  implicit val boolFromEson: FromEson[Boolean] = strict("bool") {
    case Eson.BoolVal(v) => v
  }
  implicit val unitFromEson: FromEson[Unit] = strict("unit") {
    case Eson.UnitVal => ()
  }
}

trait ToEson[-T] {
  def apply(t: T): Eson
}
object ToEson {
  implicit val bigintToEson: ToEson[BigInt]         = (t: BigInt) => Eson.IntVal(t)
  implicit val bigdecimalToEson: ToEson[BigDecimal] = (t: BigDecimal) => Eson.FloatVal(t)
  implicit val booleanToEson: ToEson[Boolean]       = (t: Boolean) => Eson.BoolVal(t)
  implicit val stringToEson: ToEson[String]         = (t: String) => Eson.TextVal(t)
  implicit val unitToEson: ToEson[Unit]             = (_: Unit) => Eson.UnitVal
  implicit def listToEson[T](implicit toEson: ToEson[T]): ToEson[Seq[T]] =
    (t: Seq[T]) => Eson.ListVal(t.map(t => toEson(t)))
  implicit def mapToEson[T](implicit toEson: ToEson[T]): ToEson[Map[String, T]] = {
    (t: Map[String, T]) =>
      Eson.DictVal(t.view.mapValues { t =>
        toEson(t)
      }.toMap)
  }
}
