package io.github.caeus.chusky.internal

import zio.{IO, RefM, ZIO}

final case class KeyedMemo[K, E, A] private (ref: RefM[Map[K, IO[E, A]]]) {
  def memo(key: K, io: IO[E, A]): IO[E, A] = {
    ref
      .modify[Any, E, IO[E, A]] { map =>
        if (map.contains(key))
          ZIO.succeed(map(key) -> map)
        else {
          io.memoize.map { memoIo =>
            memoIo -> map.updated(key, memoIo)
          }
        }
      }
      .flatten
  }
}
