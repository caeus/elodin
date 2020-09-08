package com.github.caeus.elodin.nb.archive

import com.github.caeus.elodin.nb.Elodin
import com.github.caeus.elodin.nb.archive.HArgs.&:
import com.github.caeus.elodin.nb.runtime.{Atomizer, Effect, Value}
import zio.{Task, ZIO}

object PredefArchive {
  import ArgAs._

  val eff = BookBuilder
    .withTitle("eff")
    .chapter("succeed")(
      _.when(value &: _)
        .safe {
          case value &: _ =>
            Task.succeed(Value.Atom(Effect.Result(Right(value))))
        }
    )
    .chapter("fail")(
      _.when(value &: _)
        .safe {
          case value &: _ =>
            Task.succeed(Value.Atom(Effect.Result(Left(value))))
        }
    )
    .build

  val predef = BookBuilder
    .withTitle("predef")
    .chapter("println")(
      _.when(is[String] &: _)
        .unsafe {
          case str &: _ =>
            ZIO
              .effectTotal(println(s"ELODIN SAYS: $str"))
              .map(_ => Right(Value.Atom(())))
        }
    )
    .chapter("readln")(_.unsafe { _ =>
      zio.console.Console.Service.live.getStrLn
        .bimap(
          _ => ??? : Value,
          _ => ??? : Value
        )
        .either
    })
    .chapter("readline")(_.safe { _ =>
      ZIO.accessM[Atomizer](_.get("gen", "readln"))
    })
    .chapter("concat")(
      _.when(is[String] &: is[String] &: _)
        .safe {
          case str1 &: str2 &: _ => ZIO.succeed(Value.Atom(str1 ++ str2))
        }
    )
    .build
  val gen = BookBuilder
    .withTitle("gen")
    .chapter("chain")(
      _.when(value &: fun &: _)
        .unsafe {
          case str &: fun &: _ =>
            ZIO(Right(Value.Atom(())))
        }
    )
    .chapter("done")(
      _.when(value &: _)
        .unsafe {
          case asd &: _ =>
            ZIO(Right(Value.Atom(())))
        }
    )
    .build

}
