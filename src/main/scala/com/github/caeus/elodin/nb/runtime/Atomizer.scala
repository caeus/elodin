package com.github.caeus.elodin.nb.runtime

import com.github.caeus.elodin.nb.runtime.Value.Atomic
import zio.Task

trait Atomizer {
  def get(book: String, chapter: String): Task[Value]
  def atomize(value: Value): Task[Atomic]
}
