package io.github.caeus.elodin.runtime

import PopResult.{Complete, Incomplete}
import zio._
import zio.duration._
sealed trait PopResult[+A] {
  def els: List[A]
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

  def isEmpty:UIO[Boolean] = ref.get.map(_.isEmpty)
  def elements: UIO[List[A]] = ref.get

  def pushAll(els: Iterable[A]): IO[Nothing, Unit] = {
    zio.clock.Clock.Service.live.sleep(0.milliseconds).flatMap { _ => ref.update(els.toList ::: _) }
  }

  def pop(n: Int): UIO[PopResult[A]] =
    for {
      _ <- zio.clock.Clock.Service.live.sleep(0.milliseconds)
      r <- ref.modify {
            case list if list.size >= n =>
              val asd = list.splitAt(n)
              Complete(asd._1) -> asd._2
            case list =>
              Incomplete(list) -> Nil
          }
    } yield r

}
