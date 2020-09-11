package com.github.caeus.elodin.nb.archive

import com.github.caeus.elodin.nb.archive.HArgs.{&:, Zot}
import com.github.caeus.elodin.nb.runtime.{Atomizer, EffOp, Effect, Generator, Value}
import zio.{RIO, Task, ZIO}

object BasisArchive {
  import ArgAs._

  private def nonImplemented(marker: String): ChapterBuilder[Zot] => ChapterDraft =
    _.safeM { _ =>
      Task.fail(new Exception(s" $marker UNIMPLEMENTED"))
    }
  val book: Book = BookBuilder
    .withTitle("basis")
    .chapter("error")(_.safeM { _ =>
      ZIO.fail(new Exception("MUAHAHA"))
    })
    .chapter("str.concat")(
      _.at(is[String] &: is[String] &: _)
        .safeAtom {
          case value1 &: value2 &: _ =>
            value1 ++ value2
        }
    )
    .chapter("ctrl.unpack")(
      _.at(fun &: fun &: value &: _)
        .safeM {
          case unpacker &: pipe &: arg &: _ =>
            RIO
              .accessM[Atomizer] {
                _.atomize(unpacker.apply(arg))
              }
              .map(r => pipe.apply(r))
        }
    )
    .chapter("ctrl.or")(
      _.at(value &: value &: _)
        .safeM {
          case value1 &: value2 &: _ =>
            RIO.accessM[Atomizer](
              _.atomize(value1)
                .fold(
                  _ => {
                    value2
                  },
                  identity
                )
            )
        }
    )
    .chapter("gen.done")(
      _.at(atomic &: _)
        .safeAtom {
          case result &: _ =>
            Generator.Done(result)
        }
    )
    .chapter("gen.suspend")(
      _.at(atomic &: fun &: _)
        .safeAtom {
          case atomic &: fun &: _ => Generator.Suspend(atomic, fun)
        }
    )
    .chapter("gen.un.suspend")(
      _.at(is[Generator.Suspend] &: _)
        .safeM {
          case (sus @ Generator.Suspend(step, _)) &: _ =>
            ZIO.succeed(Value.Atom(sus))
        }
    )
    .chapter("gen.un.done")(
      _.at(is[Generator.Done] &: _)
        .safeM {
          case (done @ Generator.Done(_)) &: _ =>
            ZIO.succeed(Value.Atom(done))
        }
    )
    .chapter("gen.done.result")(
      _.at(is[Generator.Done] &: _)
        .safeM {
          case Generator.Done(result: Value.Atomic) &: _ =>
            ZIO.succeed(result)
        }
    )
    .chapter("gen.suspend.step")(
      _.at(is[Generator.Suspend] &: _)
        .safeM {
          case Generator.Suspend(step, _) &: _ =>
            ZIO.succeed(step)
        }
    )
    .chapter("gen.suspend.cont")(
      _.at(is[Generator.Suspend] &: _)
        safeM {
          case Generator.Suspend(_, cont) &: _ =>
            ZIO.succeed(cont)
        }
    )
    .chapter("eff.suspend")(
      _.at(is[EffOp] &: fun &: fun &: _)
        .safeAtom {
          case op &: onSuc &: onFail &: _ =>
            Effect.Suspend(op, onSuc, onFail)
        }
    )
    .chapter("eff.succeed")(
      _.at(atomic &: _)
        .safeAtom {
          case atomic &: _ => Effect.Done(Right(atomic))
        }
    )
    .chapter("eff.fail")(
      _.at(atomic &: _)
        .safeAtom {
          case atomic &: _ => Effect.Done(Left(atomic))
        }
    )
    .chapter("eff.un.suspend")(
      _.at(is[Effect.Suspend] &: _)
        .safeM {
          case (sus @ Effect.Suspend(_, _, _)) &: _ =>
            ZIO.succeed(Value.Atom(sus))
        }
    )
    .chapter("eff.un.done")(
      _.at(is[Effect.Done] &: _)
        .safeM {
          case (sus @ Effect.Done(_)) &: _ =>
            ZIO.succeed(Value.Atom(sus))
        }
    )
    .chapter("eff.suspend.op")(
      _.at(is[Effect.Suspend] &: _)
        .safeM {
          case Effect.Suspend(op, _, _) &: _ =>
            ZIO.succeed(Value.Atom(op))
        }
    )
    .chapter("eff.suspend.scont")(
      _.at(is[Effect.Suspend] &: _)
        .safeM {
          case Effect.Suspend(_, scont, _) &: _ =>
            ZIO.succeed(scont)
        }
    )
    .chapter("eff.suspend.fcont")(
      _.at(is[Effect.Suspend] &: _)
        .safeM {
          case Effect.Suspend(_, _, fcont) &: _ =>
            ZIO.succeed(fcont)
        }
    )
    .chapter("eff.done.failure")(
      _.at(is[Effect.Done] &: _)
        .safeM {
          case Effect.Done(Left(r: Value)) &: _ =>
            ZIO.succeed(r)
        }
    )
    .chapter("eff.done.success")(
      _.at(is[Effect.Done] &: _)
        .safeM {
          case Effect.Done(Right(r: Value)) &: _ =>
            ZIO.succeed(r)
        }
    )
    .chapter("list.nil")(
      _.safeAtom { _ =>
        Nil
      }
    )
    .chapter("list.cons")(
      _.at(value &: is[List[Value]] &: _)
        .safeAtom[List[Value]] {
          case head &: tail &: _ =>
            head :: tail
        }
    )
    .chapter("dict.empty")(
      _.safeAtom[Map[String, Value]] { _ =>
        Map.empty
      }
    )
    .chapter("dict.update")(
      _.at(is[String] &: value &: is[Map[String, Value]] &: _)
        .safeAtom[Map[String, Value]] {
          case key &: value &: map &: _ =>
            map.updated(key, value)
        }
    )
    .chapter("math.plus")(
      _.at(atom &: atom &: _)
        .safeAtom {
          case (s1: BigInt) &: (s2: BigInt) &: _         => s1 + s2
          case (s1: BigInt) &: (s2: BigDecimal) &: _     => BigDecimal(s1) + s2
          case (s1: BigDecimal) &: (s2: BigInt) &: _     => s1 + BigDecimal(s2)
          case (s1: BigDecimal) &: (s2: BigDecimal) &: _ => s1 + s2
        }
    )
    .chapter("math.times")(
      _.at(atom &: atom &: _)
        .safeAtom {
          case (s1: BigInt) &: (s2: BigInt) &: _         => s1 * s2
          case (s1: BigInt) &: (s2: BigDecimal) &: _     => BigDecimal(s1) * s2
          case (s1: BigDecimal) &: (s2: BigInt) &: _     => s1 * BigDecimal(s2)
          case (s1: BigDecimal) &: (s2: BigDecimal) &: _ => s1 * s2
        }
    )
    .chapter("math.minus")(
      _.at(atom &: atom &: _)
        .safeAtom {
          case (s1: BigInt) &: (s2: BigInt) &: _         => s1 - s2
          case (s1: BigInt) &: (s2: BigDecimal) &: _     => BigDecimal(s1) - s2
          case (s1: BigDecimal) &: (s2: BigInt) &: _     => s1 - BigDecimal(s2)
          case (s1: BigDecimal) &: (s2: BigDecimal) &: _ => s1 - s2
        }
    )
    .chapter("math.divide")(
      _.at(atom &: atom &: _)
        .safeAtom {
          case (s1: BigInt) &: (s2: BigInt) &: _         => s1 / s2
          case (s1: BigInt) &: (s2: BigDecimal) &: _     => BigDecimal(s1) / s2
          case (s1: BigDecimal) &: (s2: BigInt) &: _     => s1 / BigDecimal(s2)
          case (s1: BigDecimal) &: (s2: BigDecimal) &: _ => s1 / s2
        }
    )
    .chapter("console.println")(
      _.at(is[String] &: _)
        .unsafeAtomM {
          case msg &: _ =>
            ZIO
              .effect(println(s"ELODIN SAYS: $msg"))
              .map(_ => Right(()))
        }
    )
    .chapter("console.readln")(
      _.unsafeAtomM { _ =>
        zio.console.Console.Service.live.getStrLn.map(s => Right(s))
      }
    )
    .build
}
