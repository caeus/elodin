package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.types.EloEffect
import zio.{RIO, Task}

trait EffectEngine {
  def run(desc: EloEffect.Desc): RIO[EloSystem, Either[Val, Val]]

}
final class DefaultEffectEngine extends EffectEngine {
  override def run(desc: EloEffect.Desc): RIO[EloSystem, Either[Val, Val]] = {
    desc.id match {
      case "println" =>
        RIO.environment[EloSystem].flatMap { system =>
          system.atomize(desc.args.head).flatMap {
            case Val.Atom(msg: String) =>
              println(s"MESSAGE: $msg")
              Task.succeed(Right(Val.Atom(())))
            case _ =>
              Task.succeed(Left(Val.Atom("Wrong DATA COMPADRE!")))
          }
        }
      case _ => Task.fail(new Exception("alskjd"))
    }
  }
}
