package io.github.caeus.elodin.basis

import scala.language.implicitConversions

sealed trait ValRefWrapper {
  type T
  def toValRef: ToValRef[T]
  def from: T
  final def value: ValRef = toValRef.cast(from)
}
object ValRefWrapper {
  implicit def toValRefWrapped[T0: ToValRef](t: T0): ValRefWrapper =
    new ValRefWrapper {
      override type T = T0

      override def toValRef: ToValRef[T] = implicitly[ToValRef[T]]

      override def from: T = t
    }
}
