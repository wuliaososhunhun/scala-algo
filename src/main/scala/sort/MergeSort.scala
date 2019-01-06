package sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object MergeSort {
  def main(args: Array[String]): Unit = {
    test(topDownSort[Int])
    test(bottomUpSort[Int])
    test(tailrecSort[Int])
  }

  private def test(sort: Vector[Int] => Vector[Int]): Unit = {
    val sample = Vector(10, 0, 3, 9, 2, 14, 8, 27, 1, 5, 8, -1, 26)
    val random: Vector[Int] = Random.shuffle(1 to 20).toVector
    val empty = Vector.empty[Int]
    val single = Vector(1)


    println(sort(sample))
    println(sort(random))
    println(sort(empty))
    println(sort(single))
    println("===")
  }


  def topDownSort[T: Ordering : ClassTag](vector: Vector[T]): Vector[T] = {


    def sortRec(vector: Vector[T]): Vector[T] = {

      if (vector.length <= 1) vector
      else {
        val (left, right) = vector.splitAt(vector.length / 2)
        merge(Vector.empty, sortRec(left), sortRec(right))
      }
    }

    sortRec(vector)
  }

  @tailrec
  private def merge[T: Ordering : ClassTag](result: Vector[T], left: Vector[T], right: Vector[T]): Vector[T] = {
    import scala.math.Ordering.Implicits._

    (left, right) match {
      case (IndexedSeq(), r) => result ++ r
      case (l, IndexedSeq()) => result ++ l
      case (l +: ls, r +: rs) =>
        if (l <= r) merge(result :+ l, ls, right)
        else merge(result :+ r, left, rs)
    }
  }

  def bottomUpSort[T: Ordering : ClassTag](ts: Vector[T]): Vector[T] = {

    def rec(result: Vector[T], unsorted: Vector[T]): Vector[T] = {
      if (unsorted.isEmpty) result
      else {
        val target = unsorted.take(result.length)
        rec(merge(Vector.empty, result, rec(Vector(target.head), target.tail)), unsorted.drop(result.length))
      }
    }

    ts match {
      case head +: tail => rec(Vector(head), tail)
      case _ => ts
    }
  }

  def tailrecSort[T: Ordering : ClassTag](ts: Vector[T]): Vector[T] = {

    @tailrec
    def rec(ts: Vector[T], width: Int, currentIndex: Int): Vector[T] = {
      if (width >= ts.length) ts
      else if (currentIndex >= ts.length) {
        rec(ts, width * 2, 0)
      } else {
        if (previousHasMerged(width, currentIndex)) rec(ts, width, currentIndex + width)
        else {
          val prev = ts.slice(currentIndex - width, currentIndex)
          val current = ts.slice(currentIndex, currentIndex + width)
          val merged = merge(Vector.empty, prev, current)
          val updatedVector = merged.foldLeft((ts, currentIndex - width)) { (r, newValue) =>
            val (vector, index) = r
            (vector.updated(index, newValue), index + 1)
          }._1
          rec(updatedVector, width, currentIndex + width)
        }
      }
    }

    def previousHasMerged(width: Int, startIndex: Int): Boolean = {
      require(width > 0)
      (startIndex / width) % 2 == 0
    }

    rec(ts, 1, 0)
  }
}
