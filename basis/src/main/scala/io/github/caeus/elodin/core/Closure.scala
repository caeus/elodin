package io.github.caeus.elodin.core

import io.github.caeus.elodin.core.Val.FunS

/**
  * Wrapper around FunS, in order to be able to use FromVal without covariance getting in the way
  */
sealed trait Closure{
  def fold[T](f:FunS=>T):T
}

object Closure {
  final case class ClosureImpl(private val funS: FunS) extends Closure {
    override def fold[T](f: FunS => T): T = f(funS)
  }
  def apply(funS: FunS): Closure = ClosureImpl(funS)
}
