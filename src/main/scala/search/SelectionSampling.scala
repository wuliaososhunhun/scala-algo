package search

import scala.annotation.tailrec
import scala.util.Random

object SelectionSampling {
  def main(args: Array[String]): Unit = {
    val vector = (1 to 10).toVector
    test(normal(vector, _))
    test(saveMemory(vector, _))
    test(preserveOrder(vector, _))
  }

  def test[T](impl: Int => Vector[T]): Unit = {
    println(impl(0))
    println(impl(1))
    println(impl(5))
    println(impl(9))
    println(impl(10))
    println("===")
  }

  def swap[T](v: Vector[T], i: Int, j: Int): Vector[T] = {
    val valueI = v(i)
    val valueJ = v(j)
    v.updated(i, valueJ).updated(j, valueI)
  }

  def randomIndex(low: Int, high: Int): Int = {
    require(low <= high)
    if (low == high) low
    else low + Random.nextInt(high - low)
  }


  def normal[T](v: Vector[T], k: Int): Vector[T] = {
    @tailrec
    def choose(v: Vector[T], pIndex: Int): Vector[T] = {
      val chosenIndex = randomIndex(pIndex + 1, v.length)
      if (pIndex < k) {
        val newPIndex = pIndex + 1
        choose(swap(v, chosenIndex, newPIndex), newPIndex)
      } else {
        v
      }
    }

    if (k >= v.length) v
    else {
      // choose(v, -1).take(k)
      (-1 until k).foldLeft(v){ (r, pIndex) =>
        val chosenIndex = randomIndex(pIndex + 1, v.length)
        swap(r, chosenIndex, pIndex + 1)
      }.take(k)
    }
  }

  def saveMemory[T](v: Vector[T], k: Int): Vector[T] = {
    if (k >= v.length) v
    else {
      (k until v.length).foldLeft(v.take(k)){ (r, index) =>
        val random = randomIndex(0, index)
        if (random < k) {
          r.updated(random, v(index))
        } else r
      }
    }
  }

  def preserveOrder[T](v: Vector[T], k: Int): Vector[T] = {
    if (k >= v.length) v
    else {
      v.foldLeft((Vector.empty[T], v.length)) { (tuple, e) =>
        val (result, leftSize) = tuple
        if (leftSize == 0) {
          (result, leftSize)
        } else {
          val random = Random.nextDouble() * leftSize
          if ((k - result.size) >  random) {
            (result :+ e, leftSize - 1)
          } else {
            (result, leftSize - 1)
          }
        }
      }._1
    }
  }

}
