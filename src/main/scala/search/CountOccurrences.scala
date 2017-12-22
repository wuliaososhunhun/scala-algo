package search

import scala.annotation.tailrec

/**
  * Author: yanyang.wang
  * Date: 21/12/2017
  */
object CountOccurrences {
  def main(args: Array[String]): Unit = {
    test(Array(0, 1, 1, 3, 3, 3, 3, 6, 8, 10, 11, 11))
    test(Array.emptyIntArray)
    test(Array(0))
    test(Array(0, 0))
    test(Array(0, 1))

  }

  private def test(sortedArray: Array[Int]) = {
    println("===========")
    (sortedArray.toSet + 123).foreach { e =>
      println(s"$e appears ${countByLinearRec(sortedArray, e)} times in countByLinearRec")
      println(s"$e appears ${countByBinaryWithLinear(sortedArray, e)} times in countByBinaryWithLinear")
      println(s"$e appears ${countByBinaryOnly(sortedArray, e)} times in countByBinaryOnly")
    }
  }

  def countByLinearRec[T](array: Array[T], e: T): Int = flexibleLinearCountRec(array, e, 0, array.length - 1, Forward, 0)

  sealed trait Move {
    val nextIndex: Int => Int

    def valid(startIndex: Int, endIndex: Int): Boolean
  }

  case object Forward extends Move {
    val nextIndex: Int => Int = _ + 1

    override def valid(startIndex: Int, endIndex: Int): Boolean = startIndex <= endIndex
  }

  case object Backward extends Move {
    val nextIndex: Int => Int = _ - 1

    override def valid(startIndex: Int, endIndex: Int): Boolean = startIndex >= endIndex
  }

  @tailrec
  def flexibleLinearCountRec[T](array: Array[T], e: T, pointer: Int, endIndex: Int, move: Move, count: Int): Int = {

    def inRange(target: Int, end1: Int, end2: Int): Boolean = target >= Math.min(end1, end2) && target <= Math.max(end1, end2)

    if (!inRange(pointer, 0, array.length)
      || !inRange(endIndex, 0, array.length)
      || !move.valid(pointer, endIndex)) {
      count
    } else {
      if (count > 0 && array(pointer) != e) {
        count
      } else {
        val nextIndex = move.nextIndex(pointer)
        val newCount = if (array(pointer) == e) count + 1 else count
        flexibleLinearCountRec(array, e, nextIndex, endIndex, move, newCount)
      }
    }
  }

  def countByLinearSimple[T](array: Array[T], e: T): Int = {
    // slightly worse in performance than countByLinearRec
    array.foldLeft(0) { (count, element) =>
      if (element == e) count + 1
      else count
    }
  }

  def countByBinaryWithLinear[T: Ordering](array: Array[T], e: T): Int = {
    val index = BinarySearch.indexOf(array, e)

    if (index > -1) {
      flexibleLinearCountRec(array, e, index + 1, array.length - 1, Forward, 1) +
        flexibleLinearCountRec(array, e, index - 1, 0, Backward, 1) - 1
    } else 0
  }

  def countByBinaryOnly[T: Ordering](array: Array[T], e: T): Int = {
    import scala.math.Ordering.Implicits._
    @tailrec
    def indexOfRec(lowerBound: Int, upperBound: Int, result: Int, isLeftBound: Boolean): Int = {
      if (lowerBound <= upperBound) {
        val midIndex = (lowerBound + upperBound) / 2
        // lowerBound + (upperBound - lowerBound) / 2 for large array because of overflow
        val indexElement = array(midIndex)
        if (e == indexElement) {
          if (isLeftBound) {
            indexOfRec(lowerBound, midIndex - 1, midIndex, isLeftBound)
          } else {
            indexOfRec(midIndex + 1, upperBound, midIndex, isLeftBound)
          }
        } else if (e < indexElement) {
          indexOfRec(lowerBound, midIndex - 1, result, isLeftBound)
        } else {
          indexOfRec(midIndex + 1, upperBound, result, isLeftBound)
        }
      } else {
        result
      }
    }

    val rightBound = indexOfRec(0, array.length - 1, -1, false)
    val leftBound = indexOfRec(0, array.length - 1, -1, true)
    val adjustment = if (rightBound > -1 && leftBound > -1 ) 1 else 0
    rightBound - leftBound + adjustment
  }
}
