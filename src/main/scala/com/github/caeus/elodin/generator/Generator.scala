package com.github.caeus.elodin.generator

trait Generator[-I, +D, +O] {
  def flatMap[I0 <: I, D0 >: D, O0](f: O => Generator[I0, D0, O0]): Generator[I0, D0, O0]
  def map[O0](f: O => O0): Generator[I, D, O0]
}

object ASd {
  def apply[I, I0 <: I, D, O, D0 >: D, O0](next: I => Generator[I, D, O],
                                           f: O => Generator[I0, D0, O0])(input: I0) = {
    next(input).flatMap(f)

  }
}
object Generator {
  case class Halted[-I, +D, +O](desc: D, next: I => Generator[I, D, O]) extends Generator[I, D, O] {

    override def flatMap[I0 <: I, D0 >: D, O0](
        f: O => Generator[I0, D0, O0]): Generator[I0, D0, O0] =
      Halted(desc, ASd(next, f))

    override def map[O0](f: O => O0): Generator[I, D, O0] =
      Halted(desc, { input =>
        next(input).map(f)
      })
  }

  case class Done[+O](value: O) extends Generator[Any, Nothing, O] {

    override def flatMap[I0 <: Any, D0 >: Nothing, O0](
        f: O => Generator[I0, D0, O0]): Generator[I0, D0, O0] = f(value)

    override def map[O0](f: O => O0): Generator[Any, Nothing, O0] = Done(f(value))

  }

  def task[I, D, O](desc: D, p: I => O = (i: I) => i.asInstanceOf[O]): Generator[I, D, O] =
    Halted(desc, x => Done(p(x)))

  def pure[O](t: O): Generator[Any, Nothing, O] = Done(t)

}
