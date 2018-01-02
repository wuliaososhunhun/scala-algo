package search

import scala.util.Random
import scala.math.Ordering.Implicits._

/**
  * Author: yanyang.wang
  * Date: 02/01/2018
  */
object FindMax {
  def main(args: Array[String]): Unit = {
    val array: Array[Int] = Random.shuffle(1 to 50).toArray
    println(findMax(array))
  }

  def findMax[T: Ordering](array: Array[T]): Option[T] = array.reduceLeftOption((a, b) => if (a >= b) a else b)
}
