package io.github.caeus.elodin.compile.util

import io.github.caeus.elodin.compile.util.Splitting.{Branch, Leaf}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait SplitTree[+El, Sep]

object Splitting {
  case class Branch[+El, Sep](sep: Sep, parts: List[SplitTree[El, Sep]]) extends SplitTree[El, Sep]
  case class Leaf[+El, Sep](el: El)                                      extends SplitTree[El, Sep]
}

case class SepEl[+El, +Sep](sep: Sep, el: El)
case class SepNel[+El, Sep](head: El, tail: List[SepEl[El, Sep]]) {

  def splitWhere(pred: Sep => Boolean): List[SepNel[El, Sep]] = {
    SepNel.splitRec(this, pred, Queue.empty)
  }
  def split[Sep1 >: Sep](sep: Sep1): List[SepNel[El, Sep]] = {
    SepNel.splitRec(this, (_: Sep) == sep, Queue.empty)
  }

  def splitFull(implicit ordering: Ordering[Sep]): SplitTree[El, Sep] =
    SepNel.splitFull(this)
  def separators: List[Sep] = tail.map(_.sep).distinct
}
object SepNel {

  private def splitFull[El, Sep: Ordering](curr: SepNel[El, Sep]): SplitTree[El, Sep] = {
    curr.tail match {
      case Nil => Leaf(curr.head)
      case tail =>
        val sep = tail.map(_.sep).max
        Branch(
          sep,
          curr.split(sep).map { sepnel =>
            splitFull(sepnel)
          }
        )
    }
  }

  @tailrec
  private def splitRec[El, Sep](
                                 curr: SepNel[El, Sep],
                                 pred: Sep => Boolean,
                                 q: Queue[SepNel[El, Sep]]
                               ): List[SepNel[El, Sep]] = {
    curr.tail.span { sepel =>
      !pred(sepel.sep)
    } match {
      case (_, Nil) => q.appended(curr).toList
      case (left, _head :: _tail) =>
        splitRec(SepNel(_head.el, _tail), pred, q.appended(SepNel(curr.head, left)))
    }
  }

}
