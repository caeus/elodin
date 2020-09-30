package io.github.caeus.elodin.basis

import java.util.concurrent.atomic.AtomicReference

import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.basis.Val.{FunS, TaggedS}
import zio.{IO, Semaphore, ZIO}

trait ToValRef[-T] {
  def cast(t: T): ValRef
}
object ToValRef {
  @inline
  def apply[T: ToValRef]: ToValRef[T] = implicitly[ToValRef[T]]
  implicit def valToValRef[T: ToVal]: ToValRef[T] =
    (t: T) => ValRef.fromVal(ToVal[T].cast(t))
  implicit val ValRefToValRef: ToValRef[ValRef] = (t: ValRef) => t
}

trait FromValRef[+T] { outer =>
  def accept(ref: ValRef): ZIO[ElodinEval, EvalError, T]
  final def tagged(book: String, tag: String): FromValRef[T] =
    (ref: ValRef) =>
      for {
        eval <- ZIO.environment[ElodinEval]
        value <- ref.memoEval(eval).flatMap {
                  case TaggedS(`book`, `tag`, value) => ZIO.succeed(value)
                  case p =>
                    ZIO.fail(EvalError(s"Expected a tagged($book,$tag) value, got a $p instead"))
                }
        result <- outer.accept(ValRef.fromVal(value))
      } yield result

}
object FromValRef {
  @inline
  def apply[T: FromValRef]: FromValRef[T] = implicitly[FromValRef[T]]
  implicit def tFromValRef[T: FromVal]: FromValRef[T] = { ref: ValRef =>
    ZIO.environment[ElodinEval].flatMap { eval =>
      ref
        .memoEval(eval)
        .flatMap { value =>
          ZIO.fromEither(
            FromVal[T]
              .accept(value)
              .left
              .map(_.mkString("\n"))
              .left
              .map(msg => EvalError(msg, None))
          )
        }
    }
  }
}
sealed trait ValRef { ref =>
  def memoEval(eval: ElodinEval): IO[EvalError, Val]
  final def to[T: FromValRef]: ZIO[ElodinEval, EvalError, T] = FromValRef[T].accept(ref)
}

object ValRef {
  def apply[T: ToValRef](t: T): ValRef = ToValRef[T].cast(t)

  /**
    * A Pointer that is from the start an strict value (None, if undefined)
    *
    * @param _value
    */
  final private class ResolvedValRef(_value: Either[EvalError, Val]) extends ValRef {
    override def memoEval(
        eval: ElodinEval
    ): IO[EvalError, Val] =
      ZIO.fromEither(_value)
  }

  def fromVal(value: Val): ValRef = new ResolvedValRef(Right(value))

  def error(msg: String): ValRef = new ResolvedValRef(Left(EvalError(msg, None)))

  def error(msg: String, parent: EvalError): ValRef =
    new ResolvedValRef(Left(EvalError(msg, Some(parent))))

  def fromPage(ref: ThunkRef, args: Seq[ValRef]): ZIO[Any, Nothing, ValRef] = {
    Semaphore.make(1).map { semaphore =>
      new LazyValRef(ref, args.toList, semaphore, new AtomicReference[Either[EvalError, Val]](null))
    }
  }

  def fromThunkRef(ref: ThunkRef): ZIO[Any, Nothing, ValRef] = fromPage(ref, Nil)

  final private class LazyValRef(
      thunkRef: ThunkRef,
      args: List[ValRef],
      semaphore: Semaphore,
      promise: AtomicReference[Either[EvalError, Val]]
  ) extends ValRef {
    override def memoEval(eval: ElodinEval): IO[EvalError, Val] =
      semaphore.withPermit {
        ZIO.effectSuspendTotal {
          val value1 = promise.get()
          if (value1 == null) {
            val io = eval.eval(thunkRef, args)
            io.either
              .flatMap { r => ZIO.succeed(promise.set(r)).as(r) }
              .flatMap(r => ZIO.fromEither(r))
          } else ZIO.fromEither(value1)
        }
      }
  }

}
