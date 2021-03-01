package io.github.caeus.felurian.eval

import io.github.caeus.felurian.value.Value.{Applicable, TaggedVal, typeOf}
import io.github.caeus.felurian.value.{Eson, FromEson, ToEson, Value}
import zio.{IO, ZIO}

final case class TaggedValue[Module <: String, Tag <: String, Value](
    module: Module,
    tag: Tag,
    value: Value
)

trait ToValueIO[-V] {
  def apply(v: V): ZIO[Resolver, EvalException, Value]
}
object ToValueIO {
  @inline
  def apply[T: ToValueIO]: ToValueIO[T] = implicitly[ToValueIO[T]]

  implicit def taggedValueToValueIO[
      Module <: String: ValueOf,
      Tag <: String: ValueOf,
      Value: ToValueIO
  ]: ToValueIO[TaggedValue[Module, Tag, Value]] = { v =>
    ToValueIO[Value].apply(v.value).map { v =>
      Value.TaggedVal(
        implicitly[ValueOf[Module]].value,
        implicitly[ValueOf[Tag]].value,
        v
      )
    }
  }
  implicit val valueToValueIO: ToValueIO[Value]   = v => ZIO.succeed(v)
  implicit val bigIntToValueIO: ToValueIO[BigInt] = v => ZIO.succeed(Value.IntVal(v))

  implicit val bigDecimalToValueIO: ToValueIO[BigDecimal] = v => ZIO.succeed(Value.FloatVal(v))
  implicit val booleanToValueIO: ToValueIO[Boolean]       = v => ZIO.succeed(Value.BoolVal(v))
  implicit val unitToValueIO: ToValueIO[Unit]             = _ => ZIO.succeed(Value.UnitVal)
  implicit val stringToValueIO: ToValueIO[String]         = v => ZIO.succeed(Value.TextVal(v))

  implicit def eitherToValueIO[T: ToValueIO]: ToValueIO[Either[EvalException, T]] = {
    case Right(value) => ToValueIO[T].apply(value)
    case Left(err)    => IO.fail(err)
  }
  implicit def ioToValueIO[T: ToValueIO]: ToValueIO[ZIO[Resolver, EvalException, T]] = { v =>
    v.flatMap { v =>
      ToValueIO[T].apply(v)
    }
  }
  implicit val noop: ToValueIO[ZIO[Resolver, EvalException, Value]] = v => v

  //id to id
  implicit def tEsonToValue[V](implicit toEson: ToEson[V]): ToValueIO[V] =
    (v: V) => ZIO.succeed(Value.fromEson(toEson(v)))

  implicit def listToValueIO[Item](implicit
      itemToValueIO: ToValueIO[Item]
  ): ToValueIO[Seq[Item]] =
    (v: Seq[Item]) =>
      ZIO
        .collectAll(v.map(v => itemToValueIO(v)))
        .map(v => Value.ListVal(v))

  implicit def mapToValueIO[Item](implicit
      itemToValueIO: ToValueIO[Item]
  ): ToValueIO[Map[String, Item]] =
    (v: Map[String, Item]) =>
      ZIO
        .collectAll(v.map {
          case (k, v) => itemToValueIO(v).map(k -> _)
        })
        .map(v => Value.DictVal(v.toMap))
}

trait FromValueIO[+T] {
  def apply(value: Value): ZIO[Resolver, EvalException, T]
}

object FromValueIO {
  @inline
  def apply[T: FromValueIO]: FromValueIO[T] = implicitly[FromValueIO[T]]

  private def strict[T](typeDesc: String)(f: PartialFunction[Value, T]): FromValueIO[T] =
    (value: Value) =>
      f.lift(value)
        .map(v => ZIO.succeed(v))
        .getOrElse(
          EvalException.undefined(s"Expected $typeDesc. Got ${typeOf(value)} instead")
        )

  private def strictM[T](
      typeDesc: String
  )(f: PartialFunction[Value, ZIO[Resolver, EvalException, T]]): FromValueIO[T] =
    (value: Value) =>
      f.lift(value)
        .getOrElse(
          EvalException.undefined(s"Expected $typeDesc. Got ${typeOf(value)} instead")
        )

  implicit def tagged[Module <: String: ValueOf, Tag <: String: ValueOf, V: FromValueIO]
      : FromValueIO[TaggedValue[Module, Tag, V]] =
    (value: Value) => {
      val module = implicitly[ValueOf[Module]].value
      val tag    = implicitly[ValueOf[Tag]].value
      value match {
        case TaggedVal(`module`, `tag`, value) =>
          FromValueIO[V].apply(value).map { v =>
            TaggedValue[Module, Tag, V](module, tag, v)
          }
        case _ =>
          EvalException.undefined(s"Expected tagged element. Got $value instead")
      }
    }

  implicit def eson[T](implicit fEson: FromEson[T]): FromValueIO[T] =
    (value: Value) =>
      ZIO
        .fromEither(Value.toEson(value))
        .flatMap { eson =>
          ZIO.fromEither(fEson.apply(eson))
        }
        .catchAll { errs => EvalException.undefined(errs.mkString("\n")) }

  implicit val eson: FromValueIO[Eson] = (value: Value) =>
    ZIO
      .fromEither(Value.toEson(value))
      .catchAll { errs => EvalException.undefined(errs.mkString("\n")) }
  implicit val any: FromValueIO[Value] = (value: Value) => ZIO.succeed(value)

  implicit val bigDecimal: FromValueIO[BigDecimal] = strict("floating point number") {
    case Value.FloatVal(bigDecimal: BigDecimal) => bigDecimal
  }
  implicit val bigInt: FromValueIO[BigInt] = strict("integer") {
    case Value.IntVal(bigInt: BigInt) => bigInt
  }
  implicit val string: FromValueIO[String] = strict("text") {
    case Value.TextVal(value) => value
  }

  implicit val boolean: FromValueIO[Boolean] = strict("boolean") {
    case Value.BoolVal(value) => value
  }
  implicit val unit: FromValueIO[Unit] = strict("unit") {
    case Value.UnitVal => ()
  }

  implicit val function: FromValueIO[Applicable] = strict[Applicable]("function") {
    case applicable: Applicable => applicable
  }

  implicit def map[Field](implicit
      fieldType: FromValueIO[Field]
  ): FromValueIO[Map[String, Field]] =
    strictM("dictionary") {
      case Value.DictVal(fields) =>
        ZIO
          .collectAll(fields.map {
            case (key, value) => fieldType(value).map(key -> _)
          })
          .map(_.toMap)
    }

  implicit def list[Item](implicit
      itemType: FromValueIO[Item]
  ): FromValueIO[Seq[Item]] = {
    strictM("list") {
      case Value.ListVal(items) =>
        ZIO.collectAll(items.map(item => itemType(item)))
    }
  }

}

sealed trait Nat {
  def value: Int
}
object Nat {
  final case object Zero extends Nat {
    override val value: Int = 0
  }
  final case class Succ[+Prev <: Nat](prev: Prev) extends Nat {
    override lazy val value: Int = 1 + prev.value
  }
  sealed trait NatVal[+Num <: Nat] {
    def get: Int
  }
  object NatVal {
    implicit final case object ZeroVal extends NatVal[Zero.type] {
      override val get: Int = 0
    }
    implicit def succVal[Prev <: Nat](implicit prevVal: NatVal[Prev]): NatVal[Succ[Prev]] =
      new NatVal[Succ[Prev]] {
        override val get: Int = prevVal.get + 1
      }
  }

}
