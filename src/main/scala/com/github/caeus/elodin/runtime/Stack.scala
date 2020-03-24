package com.github.caeus.elodin.runtime

import zio.{Task, UIO}

trait Stack[T] {
  def release: Task[T]
  def push(ref: T): Task[Unit]
  def pop: Task[T]
  final def popMany(take: Int): Task[Seq[T]] = {
    Task.collectAll(List.fill(take)(pop))
  }
}
object Stack {
  final class StackImpl[T](value: zio.Ref[List[T]]) extends Stack[T] {
    override def push(ref: T): Task[Unit] = value.update(ref :: _).map(_ => ())
    override def pop: Task[T] =
      value
        .modify {
          case Nil          => (None, Nil)
          case head :: tail => Some(head) -> tail
        }
        .flatMap {
          case None    => Task.fail(new IllegalStateException("Cannot pop more from stack"))
          case Some(d) => Task.succeed(d)
        }

    override def release: Task[T] = value.get.flatMap {
      case only :: Nil => Task.succeed(only)
      case _           => Task.fail(new IllegalStateException())
    }
  }
  def make[T]: UIO[Stack[T]] =
    for {
      ref <- zio.Ref.make(Nil: List[T])
    } yield new StackImpl(ref)
}
