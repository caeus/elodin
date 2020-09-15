package com.github.caeus.elodin.compile

object kajsdhkjashd {

  def asd[A, B, C](f: A => B => C, a: Any => A, b: Any => B): Any => Any => C = {

    { (anya: Any) => (anyb: Any) =>
      f(a(anya))(b(anyb))
    }
  }

}
