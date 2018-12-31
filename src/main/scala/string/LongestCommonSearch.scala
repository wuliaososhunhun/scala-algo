package string

import scala.annotation.tailrec

object LongestCommonSearch {
  def main(args: Array[String]): Unit = {
    println("==> " + search("ABCBX", "ABDCAB") + " <==")
    println("==> " + search("ABCBX", "KLMK") + " <==")
    println("==> " + search("XMJYAUZ", "MZJAWXU") + " <==")
    println("==> " + search("Hello World", "Bonjour le monde") + " <==")

  }

  def search(s1: String, s2: String): String = {
    val matrix = buildMatrix(s1, s2)

    def backTrack(matrix: Matrix[Int]): String = {

      def updateResult(r: Vector[Char], row: Int, current: Int, target: Int): Vector[Char] = {
        if (current != target) r :+ s1.charAt(row) else r
      }

      @tailrec
      def rec(result: Vector[Char], r: Int, c: Int): Vector[Char] = {
        val current = matrix.get(r, c)
        val updator: Int => Vector[Char] = updateResult(result, r, current, _)
        if (r == 0 && c == 0) updator(0)
        else {
          val up = matrix.safeGet(r - 1, c)
          val left = matrix.safeGet(r, c - 1)
          val upLeft = matrix.safeGet(r - 1, c - 1)

          (upLeft, up, left) match {
            case (None, None, Some(l)) => rec(updator(l), r, c - 1)
            case (None, Some(u), None) => rec(updator(u), r - 1, c)
            case (Some(ul), Some(u), Some(l)) =>
              val max = List(ul, u, l).max
              val (newR, newC) = if (ul == max) (r - 1, c - 1)
              else if (u == max) (r - 1, c)
              else (r, c - 1)
              rec(updator(max), newR, newC)
            case error => throw new IllegalStateException(s"unexpected state $error")
          }
        }
      }

      rec(Vector.empty, s1.length - 1, s2.length - 1).reverse.mkString("")
    }

    backTrack(matrix)
  }

  private def buildMatrix(s1: String, s2: String): Matrix[Int] = {
    def computeValue(ri: Int, ci: Int, matrix: Matrix[Int]): Int = {
      if (s1.charAt(ri) == s2.charAt(ci)) matrix.safeGet(ri - 1, ci - 1).getOrElse(0) + 1
      else Math.max(matrix.safeGet(ri - 1, ci).getOrElse(0), matrix.safeGet(ri, ci - 1).getOrElse(0))
    }

    // recursion way
    @tailrec
    def build(startR: Int, startC: Int, matrix: Matrix[Int]): Matrix[Int] = {
      if (startR >= s1.length) matrix
      else {
        val updatedMatrix = (startC until s2.length).foldLeft(matrix) {
          (result, i) =>
            val newValue = computeValue(startR, i, result)
            result.update(startR, i, newValue)
        }
        build(startR + 1, startC, updatedMatrix)
      }
    }

    val rawMatrix = Matrix.init(s1.length, s2.length, 0)
    val result = (0 until s1.length).foldLeft(rawMatrix) { (m, r) =>
      (0 until s2.length).foldLeft(m) {
        (result, i) =>
          val newValue = computeValue(r, i, result)
          result.update(r, i, newValue)
      }
    }

    result.prettyPrint(s1, s2) // debug purpose
    result
  }

}


case class Matrix[T](private val matrix: Vector[Vector[T]]) {
  def get(r: Int, c: Int): T = matrix(r)(c)

  private def safeGet[O](v: Vector[O], i: Int): Option[O] = if (i >= 0 && i < v.length) Some(v(i)) else None

  def safeGet(r: Int, c: Int): Option[T] = safeGet(matrix, r).flatMap(row => safeGet(row, c))

  def update(r: Int, c: Int, value: T): Matrix[T] = {
    val newRow = matrix(r).updated(c, value)
    Matrix(matrix.updated(r, newRow))
  }

  def prettyPrint(rowHead: String, columnHead: String): Unit =
    prettyPrint(convert(rowHead), convert(columnHead))

  private def convert(s: String): Vector[String] = s.toCharArray.map(_.toString).toVector

  def prettyPrint(rowHead: Vector[String], columnHead: Vector[String]): Unit = {
    val stringMatrix = matrix.map(_.map(_.toString))
    val table = (columnHead +: stringMatrix).zipWithIndex.map { case (a, i) =>
      val head = if (i == 0) '+' else rowHead(i - 1)
      (head +: a).mkString("\t")
    }.mkString("\n")
    println(table)
  }
}

object Matrix {
  def init[T](row: Int, column: Int, default: T): Matrix[T] =
    Matrix(Vector.fill(row)(Vector.fill(column)(default)))

  def fromVector[T](matrix: Vector[Vector[T]]): Matrix[T] = Matrix(matrix)
}
