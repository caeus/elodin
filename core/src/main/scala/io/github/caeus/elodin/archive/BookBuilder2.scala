package io.github.caeus.elodin.archive

import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.archive.ChapterBuilder.ChapterBuilderImpl
import io.github.caeus.elodin.archive.ChapterDraft.{IsAction, IsCalculation}
import io.github.caeus.elodin.archive.HArgs.{#:, Zot}
import io.github.caeus.elodin.archive.SignatureBuilder.PrependArg
import io.github.caeus.elodin.runtime.Piece._
import io.github.caeus.elodin.runtime.{Link, Piece}
import zio.{RIO, ZIO}

import scala.reflect.ClassTag

sealed trait TypedArg[+A] { self =>
  def coerce(value: Link): ZIO[ElodinEval, Unit, A]

  final def map[B](f: A => B): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: Link): ZIO[ElodinEval, Unit, B] = self.coerce(value).map(f)
    }
  final def mapSome[B](f: PartialFunction[A, B]): TypedArg[B] =
    mapSomeM { a =>
      f.lift(a).map(a => ZIO.succeed(a)).getOrElse(ZIO.fail(()))
    }
  final def mapSomeM[B](f: PartialFunction[A, ZIO[ElodinEval, Unit, B]]): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: Link): ZIO[ElodinEval, Unit, B] =
        self.coerce(value).flatMap { a: A =>
          f.lift(a).getOrElse(ZIO.fail(()))
        }
    }

}
object TypedArg {
  val link: TypedArg[Link] = new TypedArg[Link] {
    override def coerce(link: Link): ZIO[ElodinEval, Unit, Link] = ZIO.succeed(link)
  }
  val piece: TypedArg[Piece] = new TypedArg[Piece] {
    override def coerce(value: Link): ZIO[ElodinEval, Unit, Piece] =
      RIO.accessM { atomizer =>
        value.piece(atomizer)
      }
  }
  val float: TypedArg[BigDecimal] = piece.mapSome {
    case p: FloatS => p.value
  }
  val int: TypedArg[BigInt] = piece.mapSome {
    case p: IntS => p.value
  }
  val text: TypedArg[String] = piece.mapSome {
    case p: TextS => p.value
  }
  val bool: TypedArg[Boolean] = piece.mapSome {
    case p: BoolS => p.value
  }
  val unit: TypedArg[Unit] = piece.mapSome {
    case UnitS => ()
  }

  val list: TypedArg[Seq[Link]] = piece.mapSome {
    case p: ListS => p.items
  }
  def listOf[A](typedArg: TypedArg[A]): TypedArg[Seq[A]] =
    list.mapSomeM { items: Seq[Link] =>
      ZIO.collectAll(items.map(typedArg.coerce))
    }
  def dictOf[A](typedArg: TypedArg[A]): TypedArg[Map[String, A]] = {
    dict.mapSomeM { dict =>
      ZIO
        .collectAll(dict.toSeq.map {
          case (key, asd: Link) => typedArg.coerce(asd).map(key -> _)
        })
        .map(_.toMap)
    }
  }

  val dict: TypedArg[Map[String, Link]] = piece.mapSome {
    case p: DictS => p.items
  }
  val fun: TypedArg[FunS] = piece.mapSome {
    case p: FunS => p
  }
  def tagged(book: String, tag: String): TypedArg[Link] =
    piece.mapSome {
      case TaggedS(`book`, `tag`, link) => link
    }
  def foreign[T: ClassTag]: TypedArg[T] =
    piece.mapSome {
      case ForeignS(value: T) => value
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
  def take(stack: List[Link]): ZIO[ElodinEval, Unit, T]
}
object SignatureBuilder extends SignatureBuilder[Zot] { self =>
  override def arity: Int              = 0
  override def take(stack: List[Link]) = RIO.succeed(Zot)
  private final class PrependArg[H, T <: HArgs](forH: TypedArg[H], tail: SignatureBuilder[T])
      extends SignatureBuilder[H #: T] { prepSelf =>
    override lazy val arity: Int = tail.arity + 1
    override def take(stack: List[Link]) = {
      stack match {
        case h :: t =>
          forH.coerce(h).zipWithPar(tail.take(t))(_ #: _)
        case _ => ZIO.fail(())
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
  def signature: SignatureBuilder[T]
  final def at[T0 <: HArgs](f: SignatureBuilder[T] => SignatureBuilder[T0]): ChapterBuilder[T0] =
    new ChapterBuilderImpl[T0](btitle, ctitle, f(signature))

  def calculateM(f: T => ZIO[ElodinEval, Unit, Link]): ChapterDraft
  def performM(f: T => ZIO[ElodinEval, Unit, Link]): ChapterDraft

}
object ChapterBuilder {
  private final class ChapterBuilderImpl[T <: HArgs](
      val btitle: String,
      val ctitle: String,
      val signature: SignatureBuilder[T]
  ) extends ChapterBuilder[T] {
    override def calculateM(f: T => ZIO[ElodinEval, Unit, Link]): ChapterDraft = {
      IsCalculation(
        Calculate(
          signature.arity,
          form = { args =>
            signature.take(args).flatMap(f)
          }
        )
      )
    }

    override def performM(f: T => ZIO[ElodinEval, Unit, Link]): ChapterDraft = {
      IsAction(
        perform = Perform(
          arity = signature.arity,
          form = { args =>
            //Create Effect
            ???
          }
        ),
        calculate = Calculate(
          arity = signature.arity,
          form = { args =>
            ???
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
      ???
    }
  }
  def withTitle(title: String) = new BookBuilderImpl(title, Map.empty, Map.empty)
}
