package io.github.caeus.elodin.util

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object EitherUtil {

  def traverse[A, E, B](as: Seq[A])(f: A => Either[E, B]): Either[E, Seq[B]] = {
    @tailrec
    def recursive(iterator: Iterator[A],result:ListBuffer[B]):Either[E,Seq[B]]=
      if(iterator.hasNext){
        f(iterator.next()) match
          case Left(err) => Left(err)
          case Right(value) => recursive(iterator, result+=value)
      }else Right(result.toSeq)
    recursive(as.iterator,ListBuffer())
  }

  def reduceLeft[E, A](as: Seq[A])(f: (A, A) => Either[E, A]): Either[E, A] = {
    @tailrec
    def recursive(iterator: Iterator[A],result:A):Either[E,A] =
      if(iterator.hasNext)
        f(iterator.next(), result) match {
          case Left(value) => Left(value)
          case Right(value) => recursive(iterator,value)
        }
      else Right(result)
    val iterator = as.iterator
    if(iterator.hasNext){
      recursive(iterator,iterator.next())
    }else throw new NoSuchElementException("Cannot reduce on empty Seq")
  }
}
