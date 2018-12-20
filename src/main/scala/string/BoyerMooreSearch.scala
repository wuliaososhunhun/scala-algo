package string

import scala.annotation.tailrec

/**
  * Author: yanyang.wang
  * Date: 13/12/2018
  */
object BoyerMooreSearch {
  def main(args: Array[String]): Unit = {
    test(bruteForce)
    test(bmSearch)
  }

  private def test(search: (String, String) => Option[Int]): Unit = {
    val target = "Hello, World"
    val pattern = "Wor"
    println(search(target, pattern))
    println(search(target, pattern.head.toString))
    println(search(target, "l"))
    println(search(target, "w"))
    println(search(target, ""))
    println(search("", pattern))
    println(search("w", pattern))
    println("====")
  }

  def bruteForce(target: String, pattern: String): Option[Int] = {
    def isEnd(s: String, index: Int): Boolean = index == s.length - 1

    @tailrec
    def searchRec(firstMatchIndex: Option[Int], currentTargetIndex: Int, currentPatternIndex: Int): Option[Int] = {
      if (currentPatternIndex >= pattern.length || currentTargetIndex >= target.length) {
        None
      } else {
        val targetChar = target.charAt(currentTargetIndex)
        val patternChar = pattern.charAt(currentPatternIndex)
        if (targetChar == patternChar) {
          if (isEnd(pattern, currentPatternIndex)) {
            firstMatchIndex.orElse(Some(currentTargetIndex))
          } else {
            searchRec(firstMatchIndex.orElse(Some(currentTargetIndex)), currentTargetIndex + 1, currentPatternIndex + 1)
          }
        } else {
          searchRec(None, firstMatchIndex.getOrElse(currentTargetIndex) + 1, 0)
        }
      }
    }

    if (target.isEmpty || pattern.isEmpty || pattern.length > target.length) None
    else {
      searchRec(None, 0, 0)
    }
  }

  def bmSearch(target: String, pattern: String): Option[Int] = {
    if (target.isEmpty || pattern.isEmpty || pattern.length > target.length) None
    else {
      val patternLength = pattern.length
      val skipTable: Map[Char, Int] =
        pattern.zipWithIndex.map { case (c, i) => c -> (patternLength - i - 1) }.toMap

      def computeRightmostIndex(currentIndex: Int): Int =
        currentIndex + skipTable.getOrElse(target.charAt(currentIndex), patternLength)

      @tailrec
      def rec(currentRightmostIndex: Int, currentTargetIndex: Int, currentPatternIndex: Int): Option[Int] = {
        if (currentRightmostIndex >= target.length) None
        else if (currentPatternIndex >= 0) {
          val patternChar = pattern.charAt(currentPatternIndex)
          val targetChar = target.charAt(currentTargetIndex)
          if (patternChar == targetChar) {
            if (currentPatternIndex == 0) Some(currentTargetIndex)
            else {
              rec(currentRightmostIndex, currentTargetIndex - 1, currentPatternIndex - 1)
            }
          } else {
            val newRightmostIndex = computeRightmostIndex(currentRightmostIndex)
            rec(newRightmostIndex, newRightmostIndex, patternLength - 1)
          }
        } else {
          val newRightmostIndex = computeRightmostIndex(currentRightmostIndex)
          rec(newRightmostIndex, newRightmostIndex, patternLength - 1)
        }
      }

      rec(patternLength - 1, patternLength - 1, patternLength - 1)
    }
  }
}
