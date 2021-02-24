package io.github.caeus.elodin.discipline

import io.github.caeus.elodin.compile.Book
import io.github.caeus.elodin.core.{EvalError, Thunk, Val, ValRef}
import io.github.caeus.elodin.runtime.RTError
import io.github.caeus.elodin.{ElodinEval, ElodinRT}
import zio.ZIO


final case class Effect(
                         arity: Int,
                         create: List[ValRef] => ZIO[ElodinEval, EvalError, Val],
                         perform: List[Val] => ZIO[ElodinRT, RTError, Val]
                       )

sealed trait Craft {
  def title: String
  def effect(name: String): Option[Effect]
  def book: Book
}

object Craft {
  final case class NCraft(title: String, effects: Map[String, Effect]) extends Craft {
    override def effect(name: String): Option[Effect] = effects.get(name)

    override lazy val book: Book = NBook(
      title,
      `export` = effects.keys.map(s => s -> s).toMap,
      thunks = effects.map {
        case (key, effect) => key -> Thunk(effect.arity, calc = effect.create)
      }
    )
  }
}
