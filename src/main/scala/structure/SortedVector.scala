package structure

import scala.annotation.tailrec
import scala.util.Random

object SortedVector {

  def main(args: Array[String]): Unit = {
    println(Vector(1,2,3).splitAt(3))
    build(Vector(10, 0, 3, 9, 2, 14, 8, 27, 1, 5, 8, -1, 26)).print()
    build(Random.shuffle((1 to 20).toVector)).insert(22).print()
    build(Vector.empty).print()
    build(Vector(1)).print()
  }

  private def findInsertIndex[T](t: T, vector: Vector[T])(implicit ordering: Ordering[T]): Int = {

    @tailrec
    def rec(t: T, vector: Vector[T], start: Int, end: Int): Int = {
      if (start > end) start
      else {
        val middle = (start + end) / 2
        val compareResult = ordering.compare(t, vector(middle))
        if (compareResult == 0) middle
        else if (compareResult < 0) {
          rec(t, vector, start, middle - 1)
        } else {
          rec(t, vector, middle + 1, end)
        }
      }
    }

    rec(t, vector, 0, vector.length - 1)
  }

  private def insertToSortedVector[T](t: T, vector: Vector[T])(implicit ordering: Ordering[T]): Vector[T] = {
    val (pre, after) = vector.splitAt(findInsertIndex(t, vector))
    (pre :+ t) ++ after
  }

  def build[T](vector: Vector[T])(implicit ordering: Ordering[T]): SortedVector[T] = {
    vector.foldLeft(SortedVector(Vector.empty[T]))(_.insert(_))
  }
}

case class SortedVector[T] private(vector: Vector[T])(implicit ordering: Ordering[T]) {

  def insert(t: T): SortedVector[T] = SortedVector(SortedVector.insertToSortedVector(t, vector))

  def print(): Unit = println(vector)
}


