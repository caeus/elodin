package io.github.caeus.felurian.runtime

import io.github.caeus.felurian.value.Value
import io.github.caeus.felurian.value.Value.{Applicable, TaggedVal}
import io.github.caeus.felurian.runtime.Nat.Succ
import io.github.caeus.felurian.runtime.Signature.FunSig
import zio.{IO, ZIO}

trait ToValue[F[_], -V] {
  def apply(v: V): F[Value]
}
object ToValue {
  type EvalIO[V]     = ZIO[ModuleSystem, EvalException, V]
  type EvalEither[V] = Either[EvalException, V]
  type Id[V]         = V

  implicit final class AnyToValue[V](private val value: V) extends AnyVal {
    def toValueNow(implicit toValue: ToValue[Id, V]): Value = toValue(value)
    def taggedNow(module: String, tag: String*)(implicit toValue: ToValue[Id, V]) = {
      TaggedVal(module = module, tag = tag, value = toValue(value))
    }
  }

  implicit def vToValueIO[V](implicit
      toValEither: ToValue[EvalEither, V]
  ): ToValue[EvalIO, V] = { (v: V) =>
    ZIO.fromEither(toValEither(v))
  }
  implicit def vToValueEither[V](implicit toValueId: ToValue[Id, V]): ToValue[EvalEither, V] =
    (v: V) => Right(toValueId(v))

  implicit def evalIOVToEvalIO[V](implicit
      toValue: ToValue[Id, V]
  ): ToValue[EvalIO, EvalIO[V]]                     = { v => v.map(toValue.apply) }
  implicit val valueToValueId: ToValue[Id, Value]   = v => v
  implicit val bigIntToValueId: ToValue[Id, BigInt] = v => Value.IntVal(v)
  implicit def eitherVToValueEither[V](implicit
      toValue: ToValue[Id, V]
  ): ToValue[EvalEither, EvalEither[V]]                     = v => v.map(toValue.apply)
  implicit val bigDecimalToValueId: ToValue[Id, BigDecimal] = v => Value.FloatVal(v)
  implicit val booleanToValueId: ToValue[Id, Boolean]       = v => Value.BoolVal(v)
  implicit val unitToValueId: ToValue[Id, Unit]             = _ => Value.UnitVal
  implicit val stringToValueId: ToValue[Id, String]         = v => Value.TextVal(v)
  implicit def listToValue[Item](implicit
      itemToValueIO: ToValue[Id, Item]
  ): ToValue[Id, Seq[Item]] =
    (v: Seq[Item]) => Value.ListVal(v.map(itemToValueIO.apply))
  implicit def listToValueIO[Item](implicit
      itemToValueIO: ToValue[EvalIO, Item]
  ): ToValue[EvalIO, Seq[Item]] =
    (v: Seq[Item]) =>
      ZIO
        .collectAll(v.map(v => itemToValueIO(v)))
        .map(v => Value.ListVal(v))

  implicit def mapToValue[Item](implicit
      itemToValue: ToValue[Id, Item]
  ): ToValue[Id, Map[String, Item]] =
    (v: Map[String, Item]) =>
      Value.DictVal(v.map {
        case (k, v) => k -> itemToValue(v)
      })
  implicit def mapToValueIO[Item](implicit
      itemToValueIO: ToValue[EvalIO, Item]
  ): ToValue[EvalIO, Map[String, Item]] =
    (v: Map[String, Item]) =>
      ZIO
        .collectAll(v.map {
          case (k, v) => itemToValueIO(v).map(k -> _)
        })
        .map(v => Value.DictVal(v.toMap))
}
trait TypedArg[+T] { self =>
  def apply(value: Value): IO[EvalException, T]
  final def map[T0](f: T => T0): TypedArg[T0] =
    (value: Value) => self(value).map(f)

  final def mapM[T0](f: T => IO[EvalException, T0]): TypedArg[T0] =
    (value: Value) => self(value).flatMap(f)

  final def flatMap[T0](f: T => TypedArg[T0]): TypedArg[T0] =
    (value: Value) =>
      self(value).flatMap { typed =>
        f(typed)(value)
      }
}
object TypedArg {

  @inline
  def of[T](implicit typedArg: TypedArg[T]): TypedArg[T] = typedArg

  private def strict[T](typeDesc: String)(f: PartialFunction[Value, T]): TypedArg[T] =
    (value: Value) =>
      f.lift(value)
        .map(v => ZIO.succeed(v))
        .getOrElse(ZIO.fail(EvalException.Undefined(s"Expected $typeDesc. Got $value instead")))

  private def strictM[T](
      typeDesc: String
  )(f: PartialFunction[Value, IO[EvalException, T]]): TypedArg[T] =
    (value: Value) =>
      f.lift(value)
        .getOrElse(ZIO.fail(EvalException.Undefined(s"Expected $typeDesc. Got $value instead")))

  implicit val any: TypedArg[Value] = (value: Value) => ZIO.succeed(value)

  implicit val bigDecimal: TypedArg[BigDecimal] = strict("floating point number") {
    case Value.FloatVal(bigDecimal: BigDecimal) => bigDecimal
  }
  implicit val bigInt: TypedArg[BigInt] = strict("integer") {
    case Value.IntVal(bigInt: BigInt) => bigInt
  }
  implicit val string: TypedArg[String] = strict("text") {
    case Value.TextVal(value) => value
  }

  implicit val boolean: TypedArg[Boolean] = strict("boolean") {
    case Value.BoolVal(value) => value
  }
  implicit val unit: TypedArg[Unit] = strict("unit") {
    case Value.UnitVal => ()
  }

  implicit val function: TypedArg[Applicable] = strict[Applicable]("function") {
    case applicable: Applicable => applicable
  }

  implicit def map[Field](implicit fieldType: TypedArg[Field]): TypedArg[Map[String, Field]] =
    strictM("dictionary") {
      case Value.DictVal(fields) =>
        ZIO
          .collectAll(fields.map {
            case (key, value) => fieldType(value).map(key -> _)
          })
          .map(_.toMap)
    }

  implicit def list[Item](implicit itemType: TypedArg[Item]): TypedArg[Seq[Item]] = {
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
}
final case class NativeReducer[+Arity <: Nat](
    arity: Arity,
    impl: Seq[Value] => ZIO[ModuleSystem, EvalException, Value]
) {
  def orElse[NArity >: Arity <: Nat](alternative: NativeReducer[NArity]): NativeReducer[Arity] = {
    NativeReducer(
      arity,
      { args =>
        impl(args).catchSome {
          case EvalException.Undefined(_) => alternative.impl(args)
        }
      }
    )
  }
}
sealed trait Signature[Impl[_], +Arity <: Nat] { self =>
  def arity: Arity
  def impl[V](body: Impl[V])(implicit toValue: ToValue[ToValue.EvalIO, V]): NativeReducer[Arity]
  def ::[H](arg: TypedArg[H]): Signature[Signature.Fun[H, Impl]#L, Succ[Arity]] =
    FunSig[H, Impl, Arity](arg, self)
}

object Signature {

  @inline
  def of[V: TypedArg]: TypedArg[V] = TypedArg.of[V]
  type Id[V] = V
  type Fun[H, Impl[_]] = {
    type L[V] = H => Impl[V]
  }

  def apply[Impl[_], Arity <: Nat](
      build: ValueSig.type => Signature[Impl, Arity]
  ): Signature[Impl, Arity] = {
    build(ValueSig)
  }

  final case object ValueSig extends Signature[Id, Nat.Zero.type] {

    override def impl[V](
        body: V
    )(implicit toValue: ToValue[ToValue.EvalIO, V]): NativeReducer[Nat.Zero.type] =
      NativeReducer(
        Nat.Zero,
        { _ =>
          toValue(body)
        }
      )

    override def arity: Nat.Zero.type = Nat.Zero
  }

  final case class FunSig[H, Impl[_], Arity <: Nat](
      head: TypedArg[H],
      tail: Signature[Impl, Arity]
  ) extends Signature[Fun[H, Impl]#L, Succ[Arity]] { self =>
    override def arity: Succ[Arity] = Succ(tail.arity)

    override def impl[V](
        body: H => Impl[V]
    )(implicit toValue: ToValue[ToValue.EvalIO, V]): NativeReducer[Succ[Arity]] =
      NativeReducer(
        arity,
        { args =>
          head(args.head).flatMap { h: H =>
            tail.impl(body(h)).impl(args.tail)
          }
        }
      )
  }
}
