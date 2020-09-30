package io.github.caeus.elodin.eval

import io.github.caeus.elodin.archive.HArgs.#:
import io.github.caeus.elodin.archive.{BookBuilder, TypedArg}
import io.github.caeus.elodin.basis.{Archive, ThunkRef, Val, ValRef}
import io.github.caeus.elodin.{ContextElodinEval, ElodinC, ElodinEval}
import zio.ZIO
import zio.test.Assertion.{equalTo, hasField, isSubtype}
import zio.test._
object QuicksortSuites extends DefaultRunnableSpec {
  import TypedArg._

  def fib(n: BigInt): BigInt = {
    if (n.equals(BigInt(0))) {
      BigInt(0)
    } else if (n.equals(BigInt(1))) {
      BigInt(1)
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

  override def spec =
    suite("Fibonacci")(
      testM("Fibonacci") {
        val book = BookBuilder
          .withTitle("deps")
          .thunk("alskdj")(_.calculate(_ => ??? : Int))
          .thunk("if")(
            _.at(bool #: any #: any #: _)
              .calculateM {
                case cond #: ifTrue #: ifFalse #: _ =>
                  ZIO.environment[ElodinEval].flatMap { eval =>
                    (if (cond) ifTrue else ifFalse)
                      .memoEval(eval)
                  }

              }
          )
          .thunk("==")(
            _.at(is[BigInt] #: is[BigInt] #: _)
              .calculate {
                case s1 #: s2 #: _ =>
                  s1 == s2
              }
          )
          .thunk("+")(
            _.at(is[BigInt] #: is[BigInt] #: _)
              .calculate {
                case s1 #: s2 #: _ =>
                  s1 + s2
              }
          )
          .build
        val archive = Archive.make(Seq(book))

        for {
          compiler <- ElodinC.make(archive)
          book1 <- compiler.compile(
                    "test",
                    """
              |import "deps" ^{};
              |let
              |fib = (n) =>
              |  if(n==0) 0 {
              |     if(n==1)1{
              |        fib(n + -1) + fib(n+ -2)
              |     }
              |  }
              |;
              |{fib=fib}
              |""".stripMargin
                  )
          archive = Archive.make(Seq(book, book1))
          eval = new ContextElodinEval(archive, Nil)
          fibC: Val.FunS <- eval
                             .get(ThunkRef("test", "fib"))
                             .flatMap(ref=> ref.memoEval(eval))
                             .map(_.asInstanceOf[Val.FunS])
          _0  <- fibC(ValRef(0)).flatMap(ref=> ref.memoEval(eval))
          _1  <- fibC(ValRef(1)).flatMap(ref=> ref.memoEval(eval))
          _2  <- fibC(ValRef(2)).flatMap(ref=> ref.memoEval(eval))
          _3  <- fibC(ValRef(3)).flatMap(ref=> ref.memoEval(eval))
          _4  <- fibC(ValRef(4)).flatMap(ref=> ref.memoEval(eval))
          _11 <- fibC(ValRef(11)).flatMap(ref=> ref.memoEval(eval))

        } yield {
          assert(_0)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(0))))
            )
          ) && assert(_1)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(1))))
            )
          ) && assert(_2)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(2))))
            )
          ) && assert(_3)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(3))))
            )
          ) && assert(_4)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(4))))
            )
          ) && assert(_11)(
            isSubtype[Val.IntS](
              hasField("of", _.value, equalTo[Any, Any](fib(BigInt(11))))
            )
          )
        }
      }
    ) @@ TestAspect.ignore
}
