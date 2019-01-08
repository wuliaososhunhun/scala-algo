package sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object CountSort {
  def main(args: Array[String]): Unit = {
    test(sort)
  }

  private def test(sort: (Vector[Int], Range) => Vector[Int]): Unit = {
    val sample = Vector(10, 0, 3, 9, 2, 14, 8, 27, 1, 5, 8, -1, 26)
    val random: Vector[Int] = Random.shuffle((1 to 20).toVector)
    val empty = Vector.empty[Int]
    val single = Vector(1)
    val sample2 = Vector(5, 13, 2, 25, 7, 17, 20, 8, 4)

    println(sort(sample, -1 to 30))
    println(sort(random, 1 to 20))
    println(sort(empty, 0 to 10))
    println(sort(single, 0 to 2))
    println(sort(sample2, 0 to 25))
    println("===")
  }

  def sort(vector: Vector[Int], range: Range): Vector[Int] = {
    def valueToCountIndex(i: Int) = i - range.start
    def countIndexToValue(index: Int) = index + range.start

    val counts = vector.foldLeft(Vector.fill(range.length)(0)){ (r, i) =>
      if (!range.contains(i)) throw new IllegalStateException(s"$i is out of $range")
      val index = valueToCountIndex(i)
      val count = r(index)
      r.updated(index, count + 1)
    }

    counts.zipWithIndex.foldLeft((vector, 0)){ (r, tuple) =>
      val (v, currentIndex) = r
      val (count, countsIndex) = tuple
      ((currentIndex until currentIndex + count).foldLeft(v)(_.updated(_, countIndexToValue(countsIndex)))
        , currentIndex + count)
    }._1
  }

}
