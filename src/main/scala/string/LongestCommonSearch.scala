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

  private def test(search: (String, String) => List[Int]): Unit = {
    val target = "Hello, World"
    val pattern = "Wor"
    println(search(target, pattern))
    println(search(target, pattern.head.toString))
    println(search(target, "oel"))
    println(search(target, "l"))
    println(search(target, "ll"))
    println(search(target, "w"))
    println(search(target, ""))
    println(search("", pattern))
    println(search("w", pattern))
    println(search(
      "ACCCGGTTTTAAAGAACCACCATAAGATATAGACAGATATAGGACAGATATAGAGACAAAACCCCATACCCCAATATTTTTTTGGGGAGAAAAACACCACAGATAGATACACAGACTACACGAGATACGACATACAGCAGCATAACGACAACAGCAGATAGACGATCATAACAGCAATCAGACCGAGCGCAGCAGCTTTTAAGCACCAGCCCCACAAAAAACGACAATFATCATCATATACAGACGACGACACGACATATCACACGACAGCATA",
      "CATA"
    )) // [20, 64, 130, 140, 166, 234, 255, 270]
  }

  def bruteForce(target: String, pattern: String): List[Int] = {
    def isEnd(s: String, index: Int): Boolean = index == s.length - 1

    def matchString(targetIndex: Int): Boolean = {
      (0 until pattern.length)
        .zip(targetIndex until target.length)
        .takeWhile { case (pIndex, tIndex) =>
          pattern.charAt(pIndex) == target.charAt(tIndex)
        }
        .lastOption
        .exists(_._1 == pattern.length - 1)
    }

    @tailrec
    def searchRec(result: List[Int],
                  currentTargetIndex: Int): List[Int] = {
      if (currentTargetIndex + pattern.length > target.length) {
        result
      } else {
        if (matchString(currentTargetIndex)) {
          searchRec(result :+ currentTargetIndex, currentTargetIndex + 1)
        } else {
          searchRec(result, currentTargetIndex + 1)
        }
      }
    }

    if (target.isEmpty || pattern.isEmpty) Nil
    else {
      searchRec(Nil, 0)
    }
  }

  def bmSearch(target: String, pattern: String): List[Int] = {
    if (target.isEmpty || pattern.isEmpty || pattern.length > target.length) Nil
    else {
      val patternLength = pattern.length
      val skipTable: Map[Char, Int] =
        pattern.zipWithIndex.map { case (c, i) => c -> {
          val skip = patternLength - i - 1
          if (skip == 0) patternLength else skip
        }}.toMap

      def computeRightmostIndex(currentIndex: Int): Int =
        currentIndex + skipTable.getOrElse(target.charAt(currentIndex), patternLength)

      def matchString(targetIndex: Int): Option[Int] = {
        (patternLength - 1 to 0 by -1)
          .zip(targetIndex to 0 by -1)
          .takeWhile { case (pIndex, tIndex) =>
            pattern.charAt(pIndex) == target.charAt(tIndex)
          }
          .lastOption
          .filter(_._1 == 0)
          .map(_._2)
      }

      @tailrec
      def rec2(result: List[Int], currentRightmostIndex: Int): List[Int] = {
        if (currentRightmostIndex >= target.length) result
        else {
          val newRightmostIndex = computeRightmostIndex(currentRightmostIndex)
          rec2(result ++ matchString(currentRightmostIndex), newRightmostIndex)
        }
      }

      rec2(Nil, patternLength - 1)
    }
  }
}
