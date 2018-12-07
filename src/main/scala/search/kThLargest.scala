package search

import scala.annotation.tailrec
import scala.util.Random

object kThLargest {
  def main(args: Array[String]): Unit = {
    val vector = Vector(7, 92, 23, 9, -1, 0, 11, 6)
    val empty = Vector.empty[Int]
    val single = Vector(1)

    println(normal(vector, 4))
    println(normal(vector, 0))
    println(normal(vector, vector.length + 1))
    println(normal(empty, 1))
    println(normal(single, 1))
    println("===")
    println(randomized(vector, 4))
    println(randomized(vector, 0))
    println(randomized(vector, vector.length + 1))
    println(randomized(empty, 1))
    println(randomized(single, 1))
  }

  def normal[T: Ordering](v: Vector[T], k: Int): Option[T] = {
    import scala.math.Ordering.Implicits._
    if (k <= 0 || k > v.length) None
    else {
      val sorted = v.sorted
      Some(sorted(v.length - k))
    }
  }

  def randomized[T: Ordering](v: Vector[T], k: Int): Option[T] = {
    import scala.math.Ordering.Implicits._
    def randomIndex(low: Int, high: Int): Int = {
      if (low == high) low
      else low + Random.nextInt(high - low)
    }

    def swap(v: Vector[T], i: Int, j: Int): Vector[T] = {
      val valueI = v(i)
      val valueJ = v(j)
      v.updated(i, valueJ).updated(j, valueI)
    }

    @tailrec
    def partialSort(v: Vector[T], pivotIndex: Int, low: Int, high: Int): (Vector[T], Int) = {
      require(pivotIndex >= low && pivotIndex <= high)
      if (low >= high) (v, pivotIndex)
      else {
        val pivot = v(pivotIndex)
        val lowValue = v(low)
        val highValue = v(high)
        if (pivotIndex == low) {
          if (highValue < pivot) {
            partialSort(swap(v, high, pivotIndex), high, low + 1, high)
          } else {
            partialSort(v, pivotIndex, low, high - 1)
          }
        } else {
          if (lowValue > pivot) {
            val newPivotIndex = if (high == pivotIndex) low else pivotIndex
            partialSort(swap(v, low, high), newPivotIndex, low, high - 1)
          } else {
            partialSort(v, pivotIndex, low + 1, high)
          }
        }
      }
    }

    @tailrec
    def find(v: Vector[T], low: Int, high: Int, target: Int): T = {
      val pivotIndex = randomIndex(low, high)
      val (ps, i) = partialSort(v, pivotIndex, low, high)
      if (i == target) ps(i)
      else if (target < i) {
        find(ps, low, i, target)
      } else {
        find(ps, i, high, target)
      }
    }

    if (k <= 0 || k > v.length) None
    else {
      Some(find(v, 0, v.length - 1, v.length - k))
    }
  }

}
