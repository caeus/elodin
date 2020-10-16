package io.github.caeus.elodin.discipline

import io.github.caeus.elodin.archive.{Effect, NBook}
import io.github.caeus.elodin.core.{Book, Thunk}

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
        case (key, effect) => key -> Thunk(effect.arity, calc = effect.cast)
      }
    )
  }
}
