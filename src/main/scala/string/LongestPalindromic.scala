package string

import scala.annotation.tailrec

object LongestPalindromic {
  def main(args: Array[String]): Unit = {
    val sample = "forgeeksskeegfor"
    dynamic(sample).foreach(println)

  }

  def dynamic(s: String): List[String] = {
    val matrix = buildMatrix(s)
    val targets = lazyFindTopDown(s.length, matrix)
    targets.map { case (x, y) =>
      s.substring(x, y + 1)
    }
  }

  def buildMatrix(s: String): Matrix[Value] = {
    val init: Matrix[Value] = Matrix.init(s.length, s.length, Init)

    val lengthOneUpdated = (0 until s.length).foldLeft(init) { (m, i) => m.update(i, i, True) }

    val lengthTwoUpdated = (0 until s.length - 1).foldLeft(lengthOneUpdated) { (m, i) =>
      val v = if (s.charAt(i) == s.charAt(i + 1)) True else False
      m.update(i, i + 1, v)
    }

    def update(result: Matrix[Value], length: Int): Matrix[Value] = {
      (0 to (s.length - length)).foldLeft(result) { (m, i) =>
        val (x, y) = (i, i + length - 1)
        val (px, py) = (x + 1, y - 1)
        val value = if (m.safeGet(px, py).contains(True) && s.charAt(x) == s.charAt(y)) {
          True
        } else False
        m.update(x, y, value)
      }
    }

    (3 to s.length).foldLeft(lengthTwoUpdated) {
      update
    }
  }

  def lazyFindTopDown(totalLength: Int, matrix: Matrix[Value]): List[(Int, Int)] = {
    @tailrec
    def rec(length: Int): List[(Int, Int)] = {
      if (length < 1) Nil
      else findByLevel(length, totalLength, matrix) match {
        case Nil => rec(length - 1)
        case result => result
      }
    }

    rec(totalLength)
  }

  def findByLevel(length: Int, totalLength: Int, matrix: Matrix[Value]): List[(Int, Int)] = {
    (0 to (totalLength - length)).flatMap { x =>
      val y = x + length - 1
      if (matrix.safeGet(x, y).contains(True)) {
        Some((x, y))
      } else None
    }.toList
  }

}

sealed trait Value
case object Init extends Value
case object True extends Value
case object False extends Value