package com.github.caeus.elodin.nb.archive

import com.github.caeus.elodin.nb.archive.ArgsBuilder.PrependArg
import com.github.caeus.elodin.nb.archive.Book.NBook
import com.github.caeus.elodin.nb.archive.BookBuilder.BookBuilderImpl
import com.github.caeus.elodin.nb.archive.ChapterBuilder.ChapterBuilderImpl
import com.github.caeus.elodin.nb.archive.ChapterDraft.{IsAction, IsCalculation}
import com.github.caeus.elodin.nb.archive.HArgs.{&:, Zot}
import com.github.caeus.elodin.nb.runtime.Value.{Applicable, Atomic}
import com.github.caeus.elodin.nb.runtime.{Atomizer, Effect, Value}
import zio.{RIO, Task}

import scala.reflect.ClassTag

sealed trait ArgAs[+A] { self =>
  def coerce(value: Value): RIO[Atomizer, A]
  final def map[B](f: A => B): ArgAs[B] =
    new ArgAs[B] {
      override def coerce(value: Value): RIO[Atomizer, B] = self.coerce(value).map(f)
    }
  final def mapM[B](f: A => Task[B]): ArgAs[B] =
    new ArgAs[B] {
      override def coerce(value: Value): RIO[Atomizer, B] = self.coerce(value).flatMap(f)
    }
}
object ArgAs {
  val value: ArgAs[Value] = new ArgAs[Value] {
    override def coerce(value: Value): Task[Value] = Task.succeed(value)
  }
  val atomic: ArgAs[Atomic] = new ArgAs[Atomic] {
    override def coerce(value: Value): RIO[Atomizer, Atomic] = RIO.accessM(_.atomize(value))
  }
  val fun: ArgAs[Value.Fun] = atomic.mapM {
    case fun @ Value.Fun(_, _) => Task(fun)
    case _                     => Task.fail(new Exception("Type error"))
  }
  def is[T: ClassTag]: ArgAs[T] =
    atomic.mapM {
      case Value.Atom(atom: T) => Task(atom)
      case _                   => Task.fail(new Exception("Type error"))
    }

}

sealed trait HArgs {}
object HArgs {

  implicit final class HArgsOps[T <: HArgs](private val t: T) extends AnyVal {
    def &:[H](h: H) = HArgs.&:(h, t)
  }
  sealed trait Zot extends HArgs {
    def &:[A](a: A): A &: Zot
  }
  case object Zot extends Zot { self =>
    override def &:[A](a: A): A &: Zot = HArgs.&:(a, self)
  }
  case class &:[+H, +T <: HArgs](head: H, tail: T) extends HArgs {
    def &:[A](a: A): A &: H &: T = a &: head &: tail
  }
}

sealed trait ArgsBuilder[+T <: HArgs] { self =>
  def arity: Int
  final def &:[A](argAs: ArgAs[A]): ArgsBuilder[A &: T] = new PrependArg(argAs, self)
  def take(stack: List[Value]): RIO[Atomizer, T]
}
object ArgsBuilder extends ArgsBuilder[Zot] { self =>
  override def arity: Int                                         = 0
  override def take(stack: List[Value]): RIO[Atomizer, HArgs.Zot] = RIO.succeed(Zot)
  private final class PrependArg[H, T <: HArgs](forH: ArgAs[H], tail: ArgsBuilder[T])
      extends ArgsBuilder[H &: T] { prepSelf =>
    override lazy val arity: Int = tail.arity + 1
    override def take(stack: List[Value]): RIO[Atomizer, H &: T] = {
      stack match {
        case h :: t =>
          forH.coerce(h).zipWithPar(tail.take(t))(_ &: _)
        case _ => RIO.fail(new Exception("Took less than necessary"))
      }
    }
  }

}
sealed trait ChapterDraft
object ChapterDraft {
  case class IsCalculation(calculate: Calculate)              extends ChapterDraft
  case class IsAction(perform: Perform, calculate: Calculate) extends ChapterDraft
}
sealed trait ChapterBuilder[+T <: HArgs] {
  def ctitle: String
  def btitle: String
  def argsBuilder: ArgsBuilder[T]
  final def when[T0 <: HArgs](f: ArgsBuilder[T] => ArgsBuilder[T0]): ChapterBuilder[T0] =
    new ChapterBuilderImpl[T0](btitle, ctitle, f(argsBuilder))
  def safe(f: T => RIO[Atomizer, Value]): ChapterDraft
  def unsafe(f: T => RIO[Atomizer, Either[Value, Value]]): ChapterDraft
}
object ChapterBuilder {
  private final class ChapterBuilderImpl[T <: HArgs](
      val btitle: String,
      val ctitle: String,
      val argsBuilder: ArgsBuilder[T]
  ) extends ChapterBuilder[T] {
    override def safe(f: T => RIO[Atomizer, Value]): ChapterDraft = {
      IsCalculation(
        Calculate(
          arity = argsBuilder.arity,
          { args =>
            argsBuilder.take(args).flatMap(f)
          }
        )
      )
    }

    override def unsafe(f: T => RIO[Atomizer, Either[Value, Value]]): ChapterDraft = {
      IsAction(
        Perform(
          arity = argsBuilder.arity,
          { args =>
            argsBuilder.take(args).flatMap(f)
          }
        ),
        Calculate(
          arity = argsBuilder.arity,
          { args =>
            RIO
              .accessM[Atomizer] { atomizer =>
                atomizer
                  .get("eff", "succeed")
                  .flatMap(atomizer.atomize)
                  .zipPar(
                    atomizer
                      .get("eff", "fail")
                      .flatMap(atomizer.atomize)
                  )
              }
              .map {
                case (succ, fail) =>
                  Value.Atom(
                    Effect.Suspend(
                      ActionRef(btitle, ctitle),
                      args,
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
    new ChapterBuilderImpl[Zot](btitle, title, ArgsBuilder)
}

sealed trait BookBuilder {
  def chapter(title: String)(b: ChapterBuilder[Zot] => ChapterDraft): BookBuilder
  def build: Book
}
object BookBuilder {
  final class BookBuilderImpl(
      btitle: String,
      calculations: Map[String, Calculate],
      actions: Map[String, Perform]
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

object MyModule {
  import ArgAs._

  def asd: BookBuilder => Any = {
    _.chapter("alksdj")(
      _.when(value &: _)
        .safe { _ =>
          ???
        }
    )

  }

}
