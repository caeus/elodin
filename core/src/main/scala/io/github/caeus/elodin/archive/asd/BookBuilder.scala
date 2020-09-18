package io.github.caeus.elodin.archive.asd

import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.archive.Book.NBook
import io.github.caeus.elodin.archive.asd.ChapterBuilder.ChapterBuilderImpl
import io.github.caeus.elodin.archive.asd.ChapterDraft.{IsAction, IsCalculation}
import io.github.caeus.elodin.archive.asd.HArgs.{#:, Zot}
import io.github.caeus.elodin.archive.asd.SignatureBuilder.PrependArg
import io.github.caeus.elodin.archive.{ActionRef, Book, DepCalculate, DepPerform}
import io.github.caeus.elodin.runtime.Value.{Applicable, Atomic}
import io.github.caeus.elodin.runtime.{EffOp, Effect, Value}
import zio.{RIO, Task, ZIO}

import scala.reflect.ClassTag

sealed trait TypedArg[+A] { self =>
  def coerce(value: Value): RIO[ElodinEval, A]
  final def map[B](f: A => B): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: Value): RIO[ElodinEval, B] = self.coerce(value).map(f)
    }
  final def mapM[B](f: A => Task[B]): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: Value): RIO[ElodinEval, B] = self.coerce(value).flatMap(f)
    }
}
object TypedArg {
  val value: TypedArg[Value] = new TypedArg[Value] {
    override def coerce(value: Value): Task[Value] = Task.succeed(value)
  }
  val atomic: TypedArg[Atomic] = new TypedArg[Atomic] {
    override def coerce(value: Value): RIO[ElodinEval, Atomic] = RIO.accessM(_.atomize(value))
  }
  val atom: TypedArg[Any] = atomic.mapM {
    case Value.Atom(of) => Task(of)
    case x              => Task.fail(new Exception(s"Expected atom, got ${x.getClass}"))
  }
  val fun: TypedArg[Value.Fun] = atomic.mapM {
    case fun @ Value.Fun(_, _) => Task(fun)
    case x                     => Task.fail(new Exception(s"Expected function, got ${x.getClass}"))
  }
  def is[T: ClassTag]: TypedArg[T] =
    atom.mapM {
      case el: T => Task(el)
      case x =>
        Task.fail(new Exception(s"Expected ${implicitly[ClassTag[T]]} got ${x.getClass} instead "))
    }

}

sealed trait HArgs {
  def arity: Int
}
object HArgs {

  implicit final class HArgsOps[T <: HArgs](private val t: T) extends AnyVal {
    def #:[H](h: H) = HArgs.#:(h, t)
  }
  sealed trait Zot extends HArgs {
    def #:[A](a: A): A #: Zot
  }
  case object Zot extends Zot { self =>
    override def #:[A](a: A): A #: Zot = HArgs.#:(a, self)

    override def arity: Int = 0
  }
  case class #:[+H, +T <: HArgs](head: H, tail: T) extends HArgs {
    def #:[A](a: A): A #: H #: T = a #: head #: tail

    override lazy val arity: Int = 1 + tail.arity
  }
}

sealed trait SignatureBuilder[+T <: HArgs] { self =>
  def arity: Int
  final def #:[A](argAs: TypedArg[A]): SignatureBuilder[A #: T] = new PrependArg(argAs, self)
  def take(stack: List[Value]): RIO[ElodinEval, T]
}
object SignatureBuilder extends SignatureBuilder[Zot] { self =>
  override def arity: Int                                         = 0
  override def take(stack: List[Value]): RIO[ElodinEval, HArgs.Zot] = RIO.succeed(Zot)
  private final class PrependArg[H, T <: HArgs](forH: TypedArg[H], tail: SignatureBuilder[T])
      extends SignatureBuilder[H #: T] { prepSelf =>
    override lazy val arity: Int = tail.arity + 1
    override def take(stack: List[Value]): RIO[ElodinEval, H #: T] = {
      stack match {
        case h :: t =>
          forH.coerce(h).zipWithPar(tail.take(t))(_ #: _)
        case _ => RIO.fail(new Exception("Took less than necessary"))
      }
    }
  }

}
sealed trait ChapterDraft
object ChapterDraft {
  case class IsCalculation(calculate: DepCalculate)              extends ChapterDraft
  case class IsAction(perform: DepPerform, calculate: DepCalculate) extends ChapterDraft
}
sealed trait ChapterBuilder[+T <: HArgs] {
  def ctitle: String
  def btitle: String
  def argsBuilder: SignatureBuilder[T]
  final def at[T0 <: HArgs](f: SignatureBuilder[T] => SignatureBuilder[T0]): ChapterBuilder[T0] =
    new ChapterBuilderImpl[T0](btitle, ctitle, f(argsBuilder))
  def safeM(f: T => RIO[ElodinEval, Value]): ChapterDraft
  final def safe(f: T => Value): ChapterDraft = {
    safeM(t => ZIO.succeed(f(t)))
  }
  final def safeAtomM[X](f: T => RIO[ElodinEval, X]): ChapterDraft = {
    safeM { t =>
      f(t)
        .flatMap(a => ZIO.effect(Value.Atom(a)))
    }
  }
  final def safeAtom[X](f: PartialFunction[T, X]): ChapterDraft = {
    safeAtomM { t =>
      ZIO
        .effect(f.orElse[T, X]((_: T) => throw new Exception("Type error"))(t))
    }
  }
  final def unsafeAtomM[E, X](f: T => RIO[ElodinEval, Either[E, X]]): ChapterDraft = {
    unsafeM { t =>
      f(t).map(_.left.map(Value.Atom.apply).map(Value.Atom.apply))
    }
  }
  def unsafeM(f: T => RIO[ElodinEval, Either[Value, Value]]): ChapterDraft
}
object ChapterBuilder {
  private final class ChapterBuilderImpl[T <: HArgs](
      val btitle: String,
      val ctitle: String,
      val argsBuilder: SignatureBuilder[T]
  ) extends ChapterBuilder[T] {
    override def safeM(f: T => RIO[ElodinEval, Value]): ChapterDraft = {
      IsCalculation(
        DepCalculate(
          arity = argsBuilder.arity,
          { args =>
            argsBuilder
              .take(args)
              .mapError { err =>
                new Exception(s"typed error in $btitle $ctitle", err)
              }
              .flatMap(f)
          }
        )
      )
    }

    override def unsafeM(f: T => RIO[ElodinEval, Either[Value, Value]]): ChapterDraft = {
      IsAction(
        DepPerform(
          arity = argsBuilder.arity,
          { args =>
            argsBuilder.take(args).flatMap(f)
          }
        ),
        DepCalculate(
          arity = argsBuilder.arity,
          { args =>
            val value = RIO
              .accessM[ElodinEval] { atomizer =>
                atomizer
                  .get("eff", "succeed")
                  .flatMap(atomizer.atomize)
                  .zipPar(
                    atomizer
                      .get("eff", "fail")
                      .flatMap(atomizer.atomize)
                  )
              }
            value
              .map {
                case (succ, fail) =>
                  Value.Atom(
                    Effect.Suspend(
                      EffOp(ActionRef(btitle, ctitle), args),
                      succ.asInstanceOf[Applicable],
                      fail.asInstanceOf[Applicable]
                    )
                  )
              }
          }
        )
      )
    }
  }
  def fromTitle(btitle: String, title: String): ChapterBuilder[Zot] =
    new ChapterBuilderImpl[Zot](btitle, title, SignatureBuilder)
}

sealed trait BookBuilder {
  def chapter(title: String)(b: ChapterBuilder[Zot] => ChapterDraft): BookBuilder
  def build: Book
}
object BookBuilder {
  final class BookBuilderImpl(
                               btitle: String,
                               calculations: Map[String, DepCalculate],
                               actions: Map[String, DepPerform]
  ) extends BookBuilder {
    override def chapter(ctitle: String)(b: ChapterBuilder[Zot] => ChapterDraft): BookBuilder = {
      b(ChapterBuilder.fromTitle(btitle, ctitle)) match {
        case IsCalculation(calculate) =>
          new BookBuilderImpl(btitle, calculations.updated(ctitle, calculate), actions)
        case IsAction(perform, calculate) =>
          new BookBuilderImpl(
            btitle,
            calculations.updated(ctitle, calculate),
            actions.updated(ctitle, perform)
          )
      }
    }

    override def build: Book = {
      val orderedCalculations = calculations.toSeq.sortBy(_._1).toIndexedSeq
      NBook(
        btitle,
        orderedCalculations.zipWithIndex.map {
          case ((name, _), page) =>
            name -> page

        }.toMap,
        orderedCalculations.map(_._2),
        actions
      )
    }
  }
  def withTitle(title: String) = new BookBuilderImpl(title, Map.empty, Map.empty)
}

