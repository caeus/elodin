package com.github.caeus.elodin.nb.archive

import cats.{FlatMap, Monad}

object Experiment {

  sealed trait Gen[M[_], R] {
    def run(monad: FlatMap[M]): M[R]
  }
  case class Done[M[_], R](result: M[R]) extends Gen[M, R] {
    override def run(monad: FlatMap[M]): M[R] = result
  }
  case class Suspend[M[_], V, R](step: M[V], cont: V => Gen[M, R]) extends Gen[M, R] {
    override def run(monad: FlatMap[M]): M[R] =
      monad.flatMap(step) { v =>
        cont(v).run(monad)
      }
  }

  def echain[E, A, B](ea: Eff[A], faeb: A => Eff[B]): Eff[B] = {
    ???
  }

  sealed trait Op[+V] {}
  sealed trait Err         {}
  sealed trait Eff[+A] {
    def chain[B](f: A => Eff[B]): Eff[B]
  }
  case class ESuspend[V, +A](op: Op[V], scont: V => Eff[A], fcont: Err => Eff[A]) extends Eff[A] {
    def chain[B](f: A => Eff[B]): Eff[B] = {
      ESuspend[V, B](
        op,
        { v =>
          scont(v).chain(f)
        },
        { err =>
          fcont(err)
            .chain(f)
        }
      )
    }
  }
  case class EResult[+E, +A](v: Either[E, A]) extends Eff[A] {
    override def chain[B](f: A => Eff[B]): Eff[B] ={
      v match {
        case Right(value)=> f(value)
        case Left(value) => EResult(Left(value))
      }
    }
  }
}
