package io.github.caeus.runescript

import scala.annotation.tailrec
import scala.collection.BuildFrom
import java.{util => ju}

sealed trait EvalException { self: Throwable => }

case class NotDefinedAt(at: Seq[Any]) extends Throwable, EvalException

case class Id(to: String)

sealed trait Rune {}
object Rune {
  case object Unit
  trait Array {
    def isEmpty: Boolean
    def head: Any
    def tail: Array
  }
  trait Dict {}
  trait Fun {
    def apply(args: Seq[Any]): Any
  }
  case class RuneFun(scope: Scope, params: Seq[Id], body: FinalAST) extends Fun {
    def apply(args: Seq[Any]): Any = {
      Unit
    }
  }
  case class RuneArray(value: Seq[Any]) extends Array {
    def isEmpty = value.isEmpty
    def head = try value.head
    catch {
      case _: ju.NoSuchElementException => throw NotDefinedAt(Seq(this))
    }
    def tail = try RuneArray(value.tail)
    catch {
      case _: ju.NoSuchElementException => throw NotDefinedAt(Seq(this))
    }
  }
}

enum FinalAST {
  case Text(value: String)
  case Int(value: BigInt)
  case Float(value: BigDecimal)
  case Bool(value: Boolean)
  case Unit
  case Apply(fn: FinalAST, params: Seq[FinalAST])
  case Fun(args: Seq[Id], body: FinalAST)
  case Select(expr:FinalAST, field:Id)
}
object FinalAST {}

sealed trait Scope {

  def get(id: Id): Either[Throwable, Any] = ???

}
object Scope {
  case class Child(parent: Scope, bindings: Map[Id, Any]) extends Scope
  case class Root(bindings: Map[Id, Any])                 extends Scope
}
extension [Elem](seq: Seq[Elem]) {
  def traverseEither[E, B](fn: Elem => Either[E, B]): Either[E, Seq[B]] = {
    val builder  = Seq.newBuilder[B]
    val iterator = seq.iterator
    @tailrec def loop(): Either[E, Seq[B]] =
      if (iterator.hasNext) {
        fn(iterator.next) match {
          case Left(value) =>
            Left(value)
          case Right(value) =>
            builder.addOne(value)
            loop()
        }
      } else {
        Right(builder.result())
      }
    loop()
  }
}

def eval(scope: Scope, ast: FinalAST): Either[Throwable, Any] = {
  ast match {
    case FinalAST.Text(value)     => Right(value)
    case FinalAST.Int(value)      => Right(value)
    case FinalAST.Float(value)    => Right(value)
    case FinalAST.Bool(value)     => Right(value)
    case FinalAST.Unit            => Right(Rune.Unit)
    case FinalAST.Fun(args, body) => Right(Rune.RuneFun(scope = scope, params = args, body = body))
    case FinalAST.Apply(fn, params) =>
      for {
        fn     <- eval(scope, fn)
        params <- params.traverseEither(param => eval(scope, param))

      } yield ???
    case FinalAST.Select(expr,id)=> 
  }
}


/*
 * Ok, I need a roadmap.... I've tried to write a language since times impossible to remember
 * And I know I can do it, but I get stuck in the most stupid things
 * 
 * I need a function that goes String => Expr (FinalAST)
 * TODO:
 * FFI, how??? Use ETA lang and Clojure as inspiration, still need to read, don't do
 * a
 * 
 * 
 * 
 *
 */
