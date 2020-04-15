package com.github.caeus.plutus

sealed trait Slicer[Src] {
  def slice(col: Src)(from: Int, until: Int): Src
  def length(src: Src): Int
}
object Slicer {
  def apply[Src: Slicer]: Slicer[Src] = implicitly[Slicer[Src]]
  implicit object StringSlicer extends Slicer[String] {
    override def slice(col: String)(from: Int, until: Int): String = col.slice(from, until)

    override def length(src: String): Int = src.length
  }
  implicit def vectorSlicer[T]: Slicer[Vector[T]] = new Slicer[Vector[T]] {
    override def slice(col: Vector[T])(from: Int, until: Int): Vector[T] = col.slice(from, until)

    override def length(src: Vector[T]): Int = src.length
  }
}
