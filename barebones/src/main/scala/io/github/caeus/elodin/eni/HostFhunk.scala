package io.github.caeus.elodin.eni

import io.github.caeus.elodin.eni.HostFhunk.Reducer
import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.eval.{EvalException, ExceptionKind, FromValueIO, ToValueIO}
import io.github.caeus.elodin.value.Value
import zio.ZIO

sealed trait ToHostFhunk[Impl] {
  def arity: Int
  def reducer(impl: Impl): Seq[Value] => ZIO[Kernel, EvalException, Value]
  final def build(impl: Impl): HostFhunk = {
    HostFhunk(arity, reducer(impl))
  }
}

object ToHostFhunk {

  def make[Impl](_arity: Int)(
      builder: Impl => Reducer
  ): ToHostFhunk[Impl] = {
    new ToHostFhunk[Impl] {
      override def reducer(impl: Impl): Seq[Value] => ZIO[Kernel, EvalException, Value] =
        builder(impl)
      override def arity: Int = _arity
    }
  }

  implicit def function0[O](implicit toValueIO: ToValueIO[O]): ToHostFhunk[O] = {
    make(0) { o: O => _ => toValueIO(o) }
  }

  implicit def function1[I0, O](implicit
      typedArg0: FromValueIO[I0],
      o2HostFhunk: ToHostFhunk[O]
  ): ToHostFhunk[I0 => O] =
    make(o2HostFhunk.arity + 1) { (impl: I0 => O) => args =>
      val i0    = args.head
      val args_ = args.tail
      typedArg0(i0).flatMap { i0 =>
        o2HostFhunk.reducer(impl(i0)).apply(args_)
      }
    }
  implicit def function2[I0, I1, O](implicit
      typedArg0: FromValueIO[I0],
      typedArg1: FromValueIO[I1],
      o2HostFhunk: ToHostFhunk[O]
  ): ToHostFhunk[(I0, I1) => O] =
    make(o2HostFhunk.arity + 2) { (impl: (I0, I1) => O) => args =>
      val arg0  = args.head
      val arg1  = args(1)
      val args_ = args.drop(2)
      ZIO
        .mapN(typedArg0(arg0), typedArg1(arg1)) { (i0: I0, i1: I1) =>
          o2HostFhunk.reducer(impl(i0, i1)).apply(args_)
        }
        .flatten
    }

}

final case class HostFhunk private[eni] (
    arity: Int,
    reducer: Seq[Value] => ZIO[Kernel, EvalException, Value]
) {

  def orElse(other: HostFhunk) = {
    require(other.arity == this.arity)
    HostFhunk(
      arity,
      { args =>
        reducer(args).catchSome {
          case EvalException(_, ExceptionKind.Undefined, _, _) =>
            other.reducer(args)
        }
      }
    )
  }
}

object HostFhunk {
  type Reducer = Seq[Value] => ZIO[Kernel, EvalException, Value]

  def make[Impl](
      impl: Impl
  )(implicit toHostFhunk: ToHostFhunk[Impl]): HostFhunk = {
    toHostFhunk.build(impl)
  }
}
