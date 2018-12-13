package search

import scala.annotation.tailrec

/**
  * Author: yanyang.wang
  * Date: 12/12/2018
  */
object UnionFind {
  def main(args: Array[String]): Unit = {

  }


}

case class UnionFind[T](index: Map[T, Int],
                        parent: Vector[Int],
                        size: Vector[Int]) {
  def addSet(t: T): UnionFind[T] = {
    index.get(t) match {
      case None =>
        val newIndex = parent.size
        UnionFind(
          index + (t -> newIndex),
          parent :+ newIndex,
          size :+ 1
        )
      case Some(_) => throw new IllegalStateException(s"$t already exist")
    }
  }
  
  private def findRootIndex(t: T): Option[Int] = {
    @tailrec
    def findRoot(i: Int): Int = {
      val p = parent(i)
      if (p == i) i
      else findRoot(p)
    }

    index.get(t).map(findRoot)
  }

  private def inSameSet(t1: T, t2: T): Boolean = {
    (findRootIndex(t1), findRootIndex(t2)) match {
      case (Some(r1), Some(r2)) => r1 == r2
      case _ => false
    }
  }

  def union(t1: T, t2: T): UnionFind[T] = {
    val t1Root = findRootIndex(t1).getOrElse(throw new IllegalStateException(s"$t1 does not exist"))
    val t2Root = findRootIndex(t2).getOrElse(throw new IllegalStateException(s"$t1 does not exist"))
    if (t1Root == t2Root) this
    else {
      val newSize = size(t1Root) + size(t2Root)
      if (size(t1Root) < size(t2Root)) {
        UnionFind(
          index,
          parent.updated(t1Root, t2Root),
          size.updated(t2Root, newSize)
        )
      } else {
        UnionFind(
          index,
          parent.updated(t2Root, t1Root),
          size.updated(t1Root, newSize)
        )
      }
    }
  }
}