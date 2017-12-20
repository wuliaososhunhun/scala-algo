package search

/**
  * Author: yanyang.wang
  * Date: 19/12/2017
  */
object LinearSearch {
  def main(args: Array[String]): Unit = {
    val array: Array[Int] = (1 to 10).toArray

    println(indexOf(array, 4)) // random element
    println(indexOf(array, 1)) // first element
    println(indexOf(array, 10)) // last element
    println(indexOf(array, 13)) // non exist element
  }

  def indexOf[T](array: Array[T], e: T): Int = {
    for (i <- array.indices) {
      if (array(i) == e) {
        return i
      }
    }
    -1
  }
}
