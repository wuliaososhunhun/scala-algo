package sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

object HeapSort {
  def main(args: Array[String]): Unit = {
    test(sort[Int])
  }

  private def test(sort: Vector[Int] => Vector[Int]): Unit = {
    val sample = Vector(10, 0, 3, 9, 2, 14, 8, 27, 1, 5, 8, -1, 26)
    val random: Vector[Int] = Random.shuffle((1 to 20).toVector)
    val empty = Vector.empty[Int]
    val single = Vector(1)
    val sample2 = Vector(5, 13, 2, 25, 7, 17, 20, 8, 4)

    println(sort(sample))
    println(sort(random))
    println(sort(empty))
    println(sort(single))
    println(sort(sample2))
    println("===")
  }

  def sort[T: Ordering : ClassTag](vector: Vector[T]): Vector[T] = {
    import scala.math.Ordering.Implicits._
    def children(index: Int): List[Int] = List(index * 2 + 1, index * 2 + 2).filter(_ < vector.length)

    def parent(index: Int): Option[Int] = if (index == 0) None else Some((index + 1) / 2 - 1)

    @tailrec
    def appendLeaf(v: Vector[T], index: Int): Vector[T] = {
      parent(index) match {
        case Some(p) if v(p) < v(index) => appendLeaf(swap(v, p, index), p)
        case _ => v
      }
    }

    @tailrec
    def sortRoot(v: Vector[T], currentIndex: Int, endIndex: Int): Vector[T] = {
      children(currentIndex).filter(_ <= endIndex) match {
        case Nil => v
        case indices =>
          (indices :+ currentIndex).map(i => (i, v(i))).maxBy(_._2) match {
            case (i, _) if i == currentIndex => v
            case (i, _) => sortRoot(swap(v, i, currentIndex), i, endIndex)
          }
      }
    }

    val partialOrderedVector = vector.indices.foldLeft(vector)(appendLeaf)

    def recSort(v: Vector[T], nextIndex: Int): Vector[T] = {
      if (nextIndex <= 0) v
      else {
        val resorted = sortRoot(swap(v, 0, nextIndex), 0, nextIndex - 1)
        recSort(resorted, nextIndex - 1)
      }
    }

    recSort(partialOrderedVector, vector.length - 1)
  }

  private def swap[T](vector: Vector[T], i: Int, j: Int): Vector[T] = {
    val iValue = vector(i)
    val jValue = vector(j)
    vector.updated(i, jValue).updated(j, iValue)
  }
}
