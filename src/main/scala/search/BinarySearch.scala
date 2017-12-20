package search

import scala.annotation.tailrec

/**
  * Author: yanyang.wang
  * Date: 20/12/2017
  */
object BinarySearch {
  def main(args: Array[String]): Unit = {
    val sortedArray: Array[Int] = (1 to 100).toArray

    println(indexOf(sortedArray, 46)) // random element
    println(indexOf(sortedArray, 1)) // first element
    println(indexOf(sortedArray, 100)) // last element
    println(indexOf(sortedArray, 133)) // non exist element
  }

  def indexOf[T: Ordering](array: Array[T], e: T): Int = {
    import scala.math.Ordering.Implicits._
    @tailrec
    def indexOfRec(lowerBound: Int, upperBound: Int): Int = {
      lowerBound <= upperBound match {
          // if else for performance and match for readability
        case true =>
          val midIndex = (lowerBound + upperBound) / 2
          // lowerBound + (upperBound - lowerBound) / 2 for large array because of overflow
          val indexElement = array(midIndex)
          if (e == indexElement) midIndex
          else if (e < indexElement) indexOfRec(lowerBound, midIndex - 1)
          else indexOfRec(midIndex + 1, upperBound)
        case false => -1
      }
    }

    indexOfRec(0, array.length - 1)
  }
}
