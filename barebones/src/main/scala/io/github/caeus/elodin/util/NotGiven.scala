package io.github.caeus.elodin.util

sealed trait NotGiven[+T]

trait LowPriorityNotGiven {
  implicit def default[T]: NotGiven[T] = NotGiven
}
object NotGiven extends NotGiven[Nothing] with LowPriorityNotGiven {

  implicit def amb1[T](implicit ev: T): NotGiven[T] = ???
  implicit def amb2[T](implicit ev: T): NotGiven[T] = ???
}
