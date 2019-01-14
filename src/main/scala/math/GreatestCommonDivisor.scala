package math

import scala.annotation.tailrec


object GreatestCommonDivisor {
  def main(args: Array[String]): Unit = {
    println(gcd(52, 39)) // 13
    println(gcd(228, 36)) // 12
    println(gcd(51357, 3819)) // 57

    println(lcm(10, 8)) // 40
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    require(a > 0 && b > 0)
    val reminder = a % b
    if (reminder == 0) b
    else gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Int = {
    a * b / gcd(a, b)
  }
}
