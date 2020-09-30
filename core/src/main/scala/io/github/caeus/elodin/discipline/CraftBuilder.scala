package io.github.caeus.elodin.discipline

import io.github.caeus.elodin.archive.HArgs.Zot
import io.github.caeus.elodin.archive._
import io.github.caeus.elodin.basis.Val.{FunS, TaggedS}
import io.github.caeus.elodin.basis._
import io.github.caeus.elodin.discipline.Craft.NCraft
import io.github.caeus.elodin.discipline.EffectBuilder.EffectBuilderImpl
import io.github.caeus.elodin.runtime.RTError
import io.github.caeus.elodin.{ElodinEval, ElodinRT}
import zio.ZIO

sealed trait EffectBuilder[+T <: HArgs] {
  def ctitle: String
  def btitle: String
  def signature: SignatureBuilder[T]
  final def at[T0 <: HArgs](f: SignatureBuilder[T] => SignatureBuilder[T0]): EffectBuilder[T0] =
    new EffectBuilderImpl[T0](btitle, ctitle, f(signature))

  def performVal(f: T => ZIO[ElodinRT, RTError, Val]): Effect
  final def perform[V: ToVal](f: T => ZIO[ElodinRT, RTError, V]): Effect = {
    performVal { t =>
      f(t).map(ToVal[V].cast)
    }
  }

}
object EffectBuilder {
  import XXX._

  private def wrapInput(input: EffectInput): ZIO[ElodinEval, EvalError, Val] = {
    def cont(eval: ElodinEval, name: String): ZIO[Any, EvalError, FunS] = {
      eval
        .get(ThunkRef("eff", name))
        .flatMap(_.memoEval(eval))
        .flatMap {
          case f: FunS => ZIO.succeed(f)
          case p =>
            ZIO.fail(
              EvalError(
                s"Effect needs a function for its continuation, got $p instead",
                None
              )
            )
        }
    }
    for {
      eval <- ZIO.environment[ElodinEval]
      (succeed, fail) <- cont(eval, "succeed") <&>
                          cont(eval, "fail")
    } yield TaggedS("eff", "suspend", EffectSuspend(input, succeed, fail).toVal)
  }
  private final class EffectBuilderImpl[T <: HArgs](
      val btitle: String,
      val ctitle: String,
      val signature: SignatureBuilder[T]
  ) extends EffectBuilder[T] {
    override def performVal(f: T => ZIO[ElodinRT, RTError, Val]): Effect = {
      Effect(
        arity = signature.arity,
        cast = { args =>
          for {
            eval <- ZIO.environment[ElodinEval]
            args <- ZIO.collectAll(args.map(ref => ref.memoEval(eval)))
            //We ignore the signature result, we just want to memoize
            _   <- signature.take(args.map(ValRef.fromVal))
            eff <- wrapInput(EffectInput(btitle, ctitle, args))
          } yield eff
        },
        perform = { args =>
          for {
            rt <- ZIO.environment[ElodinRT]
            t <-
              signature
                .take(args.map(ValRef.fromVal))
                .provide(rt.eval)
                .mapError(_ =>
                  new RuntimeException(
                    "Args were coerced when creating effect, now they cannot be coerced when executing it"
                  )
                )
                .orDie
            result <- f(t)
          } yield result
        }
      )
    }
  }
  def fromTitle(btitle: String, etitle: String): EffectBuilder[Zot] =
    new EffectBuilderImpl[Zot](btitle, etitle, SignatureBuilder)
}

sealed trait CraftBuilder {
  def effect(title: String)(b: EffectBuilder[Zot] => Effect): CraftBuilder
  def build: Craft
}
object CraftBuilder {
  final class CraftBuilderImpl(btitle: String, effects: Map[String, Effect]) extends CraftBuilder {
    override def effect(etitle: String)(b: EffectBuilder[Zot] => Effect): CraftBuilder = {
      val effect = b(EffectBuilder.fromTitle(btitle, etitle))
      new CraftBuilderImpl(btitle, effects.updated(etitle, effect))
    }

    override def build: Craft = {
      NCraft(btitle, effects)
    }
  }
  def withName(title: String): CraftBuilder = new CraftBuilderImpl(title, effects = Map.empty)
}
