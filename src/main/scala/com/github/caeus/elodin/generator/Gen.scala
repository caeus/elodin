package com.github.caeus.elodin.generator

sealed trait Gen[M[+ _], +T] {
  def flatMap[T1](chain: T => Gen[M, T1]): Gen[M, T1]
  def run(monad: Monad[M]): M[T]
}

trait Monad[M[+ _]] {
  def flatMap[T, A, B](ma: M[A])(chain: A => M[B]): M[B]
  def pure[A](value: A): M[A]
}

object Gen {

  case class Halt[M[+_], T, V](underlying: M[V], cont: V => Gen[M, T]) extends Gen[M, T] {
    override def flatMap[T1](chain: T => Gen[M, T1]): Gen[M, T1] =
      Halt[M, T1, V](underlying, { t =>
        cont(t).flatMap(chain)
      })

    override def run(monad: Monad[M]): M[T] =
      monad.flatMap(underlying) { v =>
        cont(v).run(monad)
      }
  }
  case class Done[M[+_], T](value: T) extends Gen[M, T] {
    override def flatMap[T1](chain: T => Gen[M, T1]): Gen[M, T1] = chain(value)

    override def run(monad: Monad[M]): M[T] = monad.pure(value)
  }

}
