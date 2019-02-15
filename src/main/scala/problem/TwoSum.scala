package problem

import scala.annotation.tailrec

object TwoSum {
  // Given an array of integers and an integer target, return the indices of two numbers that add up to the target.

  def main(args: Array[String]): Unit = {
    val sample1 = Vector(2, 3, 4, 4, 7, 8, 9, 10, 12, 14, 21, 22, 100)
    println(performanceEfficient(sample1, 33))
    println(spaceEfficient(sample1, 33))
    val sample2 = Vector(5, 5, 5, 5)
    println(performanceEfficient(sample2, 10))
    println(spaceEfficient(sample2, 10))
  }


  def performanceEfficient(ints: Vector[Int], target: Int): List[(Int, Int)] = {

    def updateMap(map: Map[Int, List[Int]], key: Int, value: Int): Map[Int, List[Int]] = {
      map + (key -> (map.getOrElse(key, List.empty) :+ value))
    }

    @tailrec
    def rec(result: List[(Int, Int)], map: Map[Int, List[Int]], currentIndex: Int): List[(Int, Int)] = {
      if (currentIndex >= ints.length) {
        result
      } else {
        val current = ints(currentIndex)
        val newMap = updateMap(map, target - current, currentIndex)
        val newIndex = currentIndex + 1
        val newResult = map.get(current) match {
          case None => result
          case Some(list) => result ++ list.map((_, currentIndex))
        }

        rec(newResult, newMap, newIndex)
      }
    }

    rec(List.empty, Map.empty, 0)
  }

  // index is changed as it is sorted
  // cannot handle vector with duplicate values properly if duplicate value is part of the correct sum
  // complicated when handle duplicates scenarios like [5, 5, 5, 5] with target 10
  def spaceEfficient(ints: Vector[Int], target: Int): List[(Int, Int)] = {
    val sortedInts = ints.sorted // O(nlogn)

    @tailrec
    def rec(result: List[(Int, Int)], startIndex: Int, endIndex: Int): List[(Int, Int)] = {
      if (startIndex >= endIndex) result
      else {
        val start = ints(startIndex)
        val end = ints(endIndex)
        val diff = target - end
        if (diff > start) rec(result, startIndex + 1, endIndex)
        else if (diff < start) rec(result, startIndex, endIndex - 1)
        else {
          rec(result :+ (startIndex, endIndex), startIndex + 1, endIndex)
        }
      }
    }

    rec(List.empty, 0, ints.length - 1)
  }
}
