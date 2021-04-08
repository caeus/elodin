package io.github.caeus.elodin.util

object EitherUtil {

  def traverse[A, E, B](as: Seq[A])(f: A => Either[E, B]): Either[E, Seq[B]] =
    Right(
      as.map(a =>
        f(a) match {
          case Right(b) => b
          case Left(e)  => return Left(e)
        }
      )
    )

  def reduceLeft[E, A](as: Seq[A])(f: (A, A) => Either[E, A]): Either[E, A] = {
    Right(as.reduceLeft[A] { (a0, a1) =>
      f(a0, a1) match {
        case Right(b) => b
        case Left(b)  => return Left(b)
      }
    })
  }
}
