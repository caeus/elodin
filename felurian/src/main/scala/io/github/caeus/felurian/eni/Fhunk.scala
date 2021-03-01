package io.github.caeus.felurian.eni

import io.github.caeus.felurian.eval._
import io.github.caeus.felurian.value.Value
import zio.ZIO

sealed trait ToFhunk[-Impl] {
  def arity: Int
  def build(impl: Impl): Fhunk
}

object ToFhunk {

  final case class Impl_[-Impl](arity: Int, builder: Impl => Fhunk) extends ToFhunk[Impl] {
    override def build(impl: Impl): Fhunk = builder(impl)
  }
  private def make[Impl](
      arity: Int
  )(builder: Impl => Fhunk): ToFhunk[Impl] =
    Impl_(arity, builder)
  implicit def function0[T](implicit
      toValue: ToValueIO[T]
  ): ToFhunk[T] =
    make[T](0) { (impl: T) => Fhunk(0, _ => toValue(impl)) }

  implicit def function1[I0, O, Arity <: Nat](implicit
      typedArg0: FromValueIO[I0],
      implToFhunk: ToFhunk[O]
  ): ToFhunk[I0 => O] =
    make(implToFhunk.arity + 1) { (impl: I0 => O) =>
      {
        Fhunk(
          implToFhunk.arity + 1,
          { args =>
            val head = args.head
            typedArg0(head).flatMap { i =>
              val o = impl(i)
              implToFhunk.build(o).reducer(args.tail)
            }
          }
        )
      }
    }
  implicit def function2[I0, I1, O, Arity <: Nat](implicit
      typedArg0: FromValueIO[I0],
      typedArg1: FromValueIO[I1],
      implToFhunk: ToFhunk[O]
  ): ToFhunk[(I0, I1) => O] =
    make(implToFhunk.arity + 2) { (impl: (I0, I1) => O) =>
      {
        Fhunk(
          implToFhunk.arity + 2,
          { args =>
            val (taken, passed) = args.splitAt(2)
            ZIO
              .tupled(typedArg0(taken.head), typedArg1(taken(1)))
              .flatMap {
                case (i0, i1) =>
                  val o = impl(i0, i1)
                  implToFhunk.build(o).reducer(passed)

              }
          }
        )
      }
    }
}

final case class Fhunk private[eni] (
    arity: Int,
    reducer: Seq[Value] => ZIO[Resolver, EvalException, Value]
) {

  def tap(f: Seq[Value] => ZIO[Resolver, Nothing, Any]): Fhunk = {
    Fhunk(
      arity,
      { args =>
        f(args).flatMap { _ => reducer(args) }
      }
    )
  }

  def orElse(
      other: Fhunk
  ): Fhunk = {
    require(other.arity == arity)
    Fhunk(
      arity,
      { args =>
        reducer(args)
          .catchSome {
            case EvalException.Undefined(_) => other.reducer(args)
          }
      }
    )
  }
}

object Fhunk {
  def make[Impl](impl: Impl)(implicit toFhunk: ToFhunk[Impl]): Fhunk = {
    toFhunk.build(impl)
  }
}
