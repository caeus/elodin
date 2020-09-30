package io.github.caeus.elodin.archive

import io.github.caeus.elodin.ElodinEval
import io.github.caeus.elodin.archive.HArgs.{#:, Zot}
import io.github.caeus.elodin.archive.SignatureBuilder.PrependArg
import io.github.caeus.elodin.archive.ThunkBuilder.ThunkBuilderImpl
import io.github.caeus.elodin.basis.Val._
import io.github.caeus.elodin.basis._
import zio.{RIO, ZIO}

sealed trait TypedArg[+A] { self =>
  def coerce(value: ValRef): ZIO[ElodinEval, EvalError, A]

  final def map[B](f: A => B): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: ValRef): ZIO[ElodinEval, EvalError, B] = self.coerce(value).map(f)
    }
  final def mapM[B](f: A => ZIO[ElodinEval, EvalError, B]): TypedArg[B] =
    new TypedArg[B] {
      override def coerce(value: ValRef): ZIO[ElodinEval, EvalError, B] =
        self.coerce(value).flatMap { a: A =>
          f(a)
        }
    }

}
object TypedArg {

  @inline
  def is[T: TypedArg] = implicitly[TypedArg[T]]

  implicit val any: TypedArg[ValRef] = new TypedArg[ValRef] {
    override def coerce(link: ValRef): ZIO[ElodinEval, EvalError, ValRef] = ZIO.succeed(link)
  }
  implicit val value: TypedArg[Val] = new TypedArg[Val] {
    override def coerce(value: ValRef): ZIO[ElodinEval, EvalError, Val] =
      ZIO.accessM { eval =>
        value
          .memoEval(eval)
          .mapError { e =>
            EvalError("Expected non failed value, got error instead", Some(e))
          }
      }
  }
  implicit val float: TypedArg[BigDecimal] = value.mapM {
    case p: FloatS => ZIO.succeed(p.value)
    case p         => ZIO.fail(EvalError(s"Expected float, got ${p} instead", None))

  }
  implicit val int: TypedArg[BigInt] = value.mapM {
    case p: IntS => ZIO.succeed(p.value)
    case p       => ZIO.fail(EvalError(s"Expected int, got ${p} instead", None))

  }
  implicit val text: TypedArg[String] = value.mapM {
    case p: TextS => ZIO.succeed(p.value)
    case p        => ZIO.fail(EvalError(s"Expected text, got ${p} instead", None))

  }
  implicit val bool: TypedArg[Boolean] = value.mapM {
    case p: BoolS => ZIO.succeed(p.value)
    case p        => ZIO.fail(EvalError(s"Expected bool, got ${p} instead", None))

  }
  implicit val unit: TypedArg[Unit] = value.mapM {
    case UnitS => ZIO.succeed(())
    case p     => ZIO.fail(EvalError(s"Expected unit, got ${p} instead", None))

  }

  implicit val list: TypedArg[Seq[Val]] = value.mapM {
    case p: ListS => ZIO.succeed(p.items)
    case p        => ZIO.fail(EvalError(s"Expected list, got ${p} instead", None))

  }
  implicit def listOf[A](implicit typedArg: TypedArg[A]): TypedArg[Seq[A]] =
    list.mapM { items: Seq[Val] =>
      ZIO.collectAll(items.map(v => typedArg.coerce(ValRef.fromVal(v))))
    }
  implicit def dictOf[A](implicit typedArg: TypedArg[A]): TypedArg[Map[String, A]] = {
    dict.mapM { dict =>
      ZIO
        .collectAll(dict.toSeq.map {
          case (key, ref: Val) => typedArg.coerce(ValRef.fromVal(ref)).map(key -> _)
        })
        .map(_.toMap)
    }
  }

  implicit val dict: TypedArg[Map[String, Val]] = value.mapM {
    case p: DictS => ZIO.succeed(p.items)
    case p        => ZIO.fail(EvalError(s"Expected dict, got ${p} instead", None))

  }
  implicit val fun: TypedArg[FunS] = value.mapM {
    case p: FunS => ZIO.succeed(p)
    case p       => ZIO.fail(EvalError(s"Expected function, got ${p} instead", None))
  }
  def tagged(book: String, tag: String): TypedArg[Val] =
    value.mapM {
      case TaggedS(`book`, `tag`, link) => ZIO.succeed(link)
      case p                            => ZIO.fail(EvalError(s"""Expected tagged("$book"\\$tag), got ${p} instead""", None))

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
  def take(stack: List[ValRef]): ZIO[ElodinEval, EvalError, T]

}
object SignatureBuilder extends SignatureBuilder[Zot] { self =>
  override def arity: Int = 0
  override def take(stack: List[ValRef]) = {
    stack match {
      case Nil => RIO.succeed(Zot)
      case _ =>
        println("Taking more than it should, WHY??")
        ZIO.fail(EvalError("Taking more than it should, WHY??", None))
    }
  }

  private final class PrependArg[H, T <: HArgs](forH: TypedArg[H], tail: SignatureBuilder[T])
      extends SignatureBuilder[H #: T] { prepSelf =>
    override lazy val arity: Int = tail.arity + 1
    override def take(stack: List[ValRef]) = {
      stack match {
        case h :: t =>
          forH.coerce(h).zipWith(tail.take(t))(_ #: _)
        case _ => ZIO.fail(EvalError(s"Got empty arg list, expecting at least $arity values", None))
      }
    }
  }

}

sealed trait ThunkBuilder[+T <: HArgs] {
  def ctitle: String
  def btitle: String
  def signature: SignatureBuilder[T]
  final def at[T0 <: HArgs](f: SignatureBuilder[T] => SignatureBuilder[T0]): ThunkBuilder[T0] =
    new ThunkBuilderImpl[T0](btitle, ctitle, f(signature))

  def calculateVal(f: T => ZIO[ElodinEval, EvalError, Val]): Thunk
  final def calculateM[V: ToVal](f: T => ZIO[ElodinEval, EvalError, V]): Thunk = {
    calculateVal { t =>
      f(t).map(ToVal[V].cast)
    }
  }
  final def calculate[V: ToVal](f: T => V): Thunk = {
    calculateVal { t =>
      ZIO.succeed(ToVal[V].cast(f(t)))
    }
  }
}
object ThunkBuilder {
  private final class ThunkBuilderImpl[T <: HArgs](
      val btitle: String,
      val ctitle: String,
      val signature: SignatureBuilder[T]
  ) extends ThunkBuilder[T] {

    override def calculateVal(f: T => ZIO[ElodinEval, EvalError, Val]): Thunk = {
      Thunk(
        signature.arity,
        calc = { args =>
          signature
            .take(args)
            .flatMap(f)
        }
      )
    }

  }
  def fromTitle(btitle: String, title: String): ThunkBuilder[Zot] =
    new ThunkBuilderImpl[Zot](btitle, title, SignatureBuilder)
}

sealed trait BookBuilder {
  def thunk(title: String)(b: ThunkBuilder[Zot] => Thunk): BookBuilder
  def build: Book
}
object BookBuilder {
  final class BookBuilderImpl(
      btitle: String,
      calculations: Map[String, Thunk]
  ) extends BookBuilder {
    override def thunk(ttitle: String)(b: ThunkBuilder[Zot] => Thunk): BookBuilder = {
      val thunk = b(ThunkBuilder.fromTitle(btitle, ttitle))
      new BookBuilderImpl(btitle, calculations.updated(ttitle, thunk))
    }

    override def build: Book = {
      NBook(btitle, calculations.map(s => s._1 -> s._1), calculations)
    }
  }
  def withTitle(title: String) = new BookBuilderImpl(title, Map.empty)
}
