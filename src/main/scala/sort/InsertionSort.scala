package sort

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

/**
  * Author: yanyang.wang
  * Date: 20/12/2017
  */
object InsertionSort {
  def main(args: Array[String]): Unit = {
    val sample = Array(10, -1, 3, 9, 2, 27, 8, 5, 1, 3, 0, 26)
    val random: Array[Int] = Random.shuffle((1 to 20).toList).toArray
    val empty = Array.empty[Int]
    val single = Array(1)
    println(multiArray(sample).toList)
    println(multiArray(random).toList)
    println(multiArray(empty).toList)
    println(multiArray(single).toList)

    println(inArray(sample).toList)
    println(inArray(random).toList)
    println(inArray(empty).toList)
    println(inArray(single).toList)
  }

  def multiArray[T: Ordering : ClassTag](array: Array[T]): Array[T] = {
    import scala.math.Ordering.Implicits._
    def insertToArray(t: T, sortedArray: Array[T]): Array[T] = {
      @tailrec
      def insertRec(t: T, unprocessed: Array[T], result: Array[T]): Array[T] = {
        unprocessed match {
          case Array() => result :+ t
          case old@Array(head, _*) if head >= t => (result :+ t) ++ old
          case Array(head, tail@_*) => insertRec(t, tail.toArray, result :+ head)
        }
      }

      insertRec(t, sortedArray, Array.empty)
    }

    def insertToArrayByFold(t: T, sortedArray: Array[T]): Array[T] = {
      sortedArray.foldLeft((Array.empty[T], false)) { (tuple, e) =>
        val r = tuple._1
        val isInserted = tuple._2
        if (isInserted) (r :+ e, isInserted)
        else {
          if (e < t) {
            (r :+ e, isInserted)
          } else {
            (r :+ t :+ e, true)
          }
        }
      }._1
    }

    @tailrec
    def sortRec(unsorted: Array[T], result: Array[T]): Array[T] = {
      unsorted match {
        case Array() => result
        case Array(head, tail@_*) => sortRec(tail.toArray, insertToArray(head, result))
      }
    }

    // sortRec(array, Array.empty)
    array.foldLeft(Array.empty[T]) { (r, e) => insertToArray(e, r) }
  }


  def inArray[T: Ordering : ClassTag](array: Array[T]): Array[T] = {
    import scala.math.Ordering.Implicits._
      @tailrec
      def rec(array: Array[T], barIndex: Int, candidateIndex: Option[Int]): Array[T] = {
        // can store candidate value in the parameter.
        candidateIndex match {
          case None =>
            if (barIndex < array.length) {
              rec(array, barIndex, Some(barIndex))
            } else {
              array
            }
          case Some(p) =>
            if (p > 0) {
              val target = array(p)
              val largest = array(p - 1)
              if (target >= largest) {
                rec(array, barIndex + 1, None)
              } else {
                array.update(p, largest)
                array.update(p - 1, target)
                rec(array, barIndex, Some(p - 1))
              }
            } else {
              rec(array, barIndex + 1, None)
            }
        }
      }
    rec(array, 0, None)
  }
}
