package com.github.caeus.elodin.compile

import com.github.caeus.elodin.archive.Book.CBook
import com.github.caeus.elodin.{Elodin, ElodinC}
import com.github.caeus.elodin.archive.{Archive, ArgAs, BookBuilder, Calculate}
import com.github.caeus.elodin.archive.HArgs.#:
import com.github.caeus.elodin.runtime.{PathdAtomizer, Value}
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object AssemblerSuites extends DefaultRunnableSpec {
  import ArgAs._

  def fib(n: BigInt): BigInt = {
    if (n.equals(BigInt(0))) {
      BigInt(0)
    } else if (n.equals(BigInt(1))) {
      BigInt(1)
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("Assembler")(
      testM("Fibonacci") {
        val book = BookBuilder
          .withTitle("deps")
          .chapter("if")(
            _.at(is[Boolean] #: value #: value #: _)
              .safe {
                case cond #: ifTrue #: ifFalse #: _ =>
                  if (cond) ifTrue else ifFalse
              }
          )
          .chapter("==")(
            _.at(is[BigInt] #: is[BigInt] #: _)
              .safeAtom {
                case s1 #: s2 #: _ =>
                  s1 == s2
              }
          )
          .chapter("+")(
            _.at(is[BigInt] #: is[BigInt] #: _)
              .safeAtom {
                case s1 #: s2 #: _ =>
                  s1 + s2
              }
          )
          .build
        for {
          archive  <- Archive.make(Seq(book))
          compiler <- ElodinC.make(archive)
          book1 <- compiler.compile(
                    "test",
                    """
              |import "deps" ^{};
              |let
              |fib = (n) =>
              |  if(n == 0) 0 {
              |     if(n == 1) 1 {
              |       fib(n + -1) + fib(n + -2)
              |     }
              |  }
              |;
              |{fib=fib}
              |""".stripMargin
                  )
          archive <- Archive.make(Seq(book, book1))
          atomizer = new PathdAtomizer(archive, Nil)
          fibC <- atomizer
                   .get("test", "fib")
                   .flatMap(atomizer.atomize)
                   .map(_.asInstanceOf[Value.Fun])
          _0 <- atomizer.atomize(fibC(Value.Atom(BigInt(0))))
          _1 <- atomizer.atomize(fibC(Value.Atom(BigInt(1))))
          _2 <- atomizer.atomize(fibC(Value.Atom(BigInt(2))))
          _3 <- atomizer.atomize(fibC(Value.Atom(BigInt(3))))
          _4 <- atomizer.atomize(fibC(Value.Atom(BigInt(4))))
          _5 <- atomizer.atomize(fibC(Value.Atom(BigInt(5))))
          _6 <- atomizer.atomize(fibC(Value.Atom(BigInt(6))))
        } yield {
          assert(_0)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(0))))
            )
          ) && assert(_1)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(1))))
            )
          ) && assert(_2)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(2))))
            )
          ) && assert(_3)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(3))))
            )
          ) && assert(_4)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(4))))
            )
          ) && assert(_5)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(5))))
            )
          ) && assert(_6)(
            isSubtype[Value.Atom](
              hasField("of", _.of, equalTo[Any, Any](fib(BigInt(6))))
            )
          )
        }
      }
    )
}
