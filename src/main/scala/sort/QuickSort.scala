package sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object QuickSort {
  def main(args: Array[String]): Unit = {
    test(copySort[Int])
    test(inplaceSort[Int])
  }

  private def test(sort: Vector[Int] => Vector[Int]): Unit = {
    val sample = Vector(10, 0, 3, 9, 2, 14, 8, 27, 1, 5, 8, -1, 26)
    val random: Vector[Int] = Random.shuffle((1 to 20).toVector)
    val empty = Vector.empty[Int]
    val single = Vector(1)


    println(sort(sample).toList)
    println(sort(random).toList)
    println(sort(empty).toList)
    println(sort(single).toList)
    println("===")
  }


  def copySort[T: Ordering : ClassTag](vector: Vector[T]): Vector[T] = {
    import scala.math.Ordering.Implicits._
    def sortRec(vector: Vector[T]): Vector[T] = {

      def partialSort(vector: Vector[T]): (Vector[T], T, Vector[T]) = {
        require(vector.length > 1)
        val pivot = vector.last
        val (before, after) = vector.dropRight(1).foldLeft((Vector.empty[T], Vector.empty[T])) { (r, v) =>
          val (before, after) = r
          if (v <= pivot) (before :+ v, after)
          else (before, after :+ v)
        }
        (before, pivot, after)

      }

      if (vector.size <= 1) vector
      else {
        val pivotIndex = Random.nextInt(vector.length)
        val newVector = swap(vector, pivotIndex, vector.length - 1)
        val (before, pivot, after) = partialSort(newVector)
        (sortRec(before) :+ pivot) ++ sortRec(after)
      }
    }

    sortRec(vector)
  }

  private def swap[T](vector: Vector[T], i: Int, j: Int): Vector[T] = {
    val iValue = vector(i)
    val jValue = vector(j)
    vector.updated(i, jValue).updated(j, iValue)
  }

  def inplaceSort[T: Ordering : ClassTag](ts: Vector[T]): Vector[T] = {
    import scala.math.Ordering.Implicits._

    @tailrec
    def partialSort(processedTs: Vector[T], startIndex: Int, endIndex: Int, pivotIndex: Int): (Vector[T], Int) = {
      require(startIndex <= endIndex)

      val start = processedTs(startIndex)
      val pivot = processedTs(pivotIndex)

      if (startIndex == endIndex) {
        if (start <= pivot) {
          (swap(processedTs, startIndex + 1, pivotIndex), startIndex + 1)
        } else {
          (swap(processedTs, startIndex, pivotIndex), startIndex)
        }
      } else {
        if (start <= pivot) {
          partialSort(processedTs, startIndex + 1, endIndex, pivotIndex)
        } else {
          partialSort(swap(processedTs, startIndex, endIndex), startIndex, endIndex - 1, pivotIndex)
        }
      }
    }

    @tailrec
    def rec(ts: Vector[T], unsortedRanges: List[(Int, Int)]): Vector[T] = {
      unsortedRanges match {
        case Nil => ts
        case head :: tail =>
          val (nextStart, nextEnd) = head
          if (nextStart < nextEnd) {
            val pivotIndex = nextStart + Random.nextInt(nextEnd - nextStart + 1)
            val processedVector = swap(ts, pivotIndex, nextEnd)
            val (partialSortVector, newPivotIndex) = partialSort(processedVector, nextStart, nextEnd - 1, nextEnd)
            rec(partialSortVector, (nextStart, newPivotIndex - 1) :: (newPivotIndex + 1, nextEnd) :: tail)
          } else rec(ts, tail)
      }
    }

    rec(ts, List((0, ts.length - 1)))
  }
}
