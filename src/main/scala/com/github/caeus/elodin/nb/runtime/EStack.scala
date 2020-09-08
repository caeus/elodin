package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.runtime.PopResult.{Complete, Incomplete}
import zio._

sealed trait PopResult[+A] {
  def els:List[A]
}
object PopResult {
  final case class Incomplete[+A](els: List[A]) extends PopResult[A]
  final case class Complete[+A](els: List[A])   extends PopResult[A]
}
object EStack {
  def make[A](init: List[A]): UIO[EStack[A]] =
    Ref
      .make(init)
      .map { ref =>
        new EStack[A](ref)
      }
}
final class EStack[A](ref: zio.Ref[List[A]]) {

  def elements: UIO[List[A]] = ref.get
  def push(el: A): UIO[Unit] =
    ref.update(el :: _)

  def pushAll(els: Iterable[A]) =
    ref.update(els.toList ::: _)

  def pop(n: Int): UIO[PopResult[A]] =
    for {
      r <- ref.modify {
            case list if list.size >= n =>
              val asd = list.splitAt(n)
              Complete(asd._1) -> asd._2
            case list =>
              Incomplete(list) -> Nil
          }
    } yield r

}
