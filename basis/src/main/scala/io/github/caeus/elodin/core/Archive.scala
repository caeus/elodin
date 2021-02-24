package io.github.caeus.elodin.core

trait Archive {
  def thunkRefTo(module: String, ref: String): Option[ThunkRef]
  def thunkAt(ref: ThunkRef): Option[Thunk]
  def enrichedWith(other: Archive): Archive
}

final class Archive1()

trait ArchiveIndex {
  def of(module: String): Option[Set[String]]
  def contentRef(module: String, name: String): Option[ThunkRef]

}
trait ArchiveContent {
  def thunk(ref: ThunkRef): Option[Thunk]
}
