package problem

object MinCoinChange {
  // Given a value V
  // if we want to make change for V cents
  // and we have infinite supply of each of C = { C1, C2, .. , Cm} valued coins
  // what is the minimum number of coins to make the change?
  // Examples:
  //  Input: coins[] = {25, 10, 5}, V = 30    Output: 2
  //  Input: coins[] = {9, 6, 5, 1}, V = 11   Output: 2
  //  Input: coins[] = {4, 3, 1}, V = 6       Output: 2

  def main(args: Array[String]): Unit = {
    println(greedy(30, List(25, 10, 5)))
    println(greedy(11, List(9, 6, 5, 1)))
    println(greedy(6, List(4, 3, 1)))

    println("===")
    
    println(dynamic(30, List(25, 10, 5)))
    println(dynamic(11, List(9, 6, 5, 1)))
    println(dynamic(6, List(4, 3, 1)))
  }

  def greedy(v: Int, coins: List[Int]): Option[Int] = {
    require(v >= 0)
    require(coins.forall(_ > 0))

    val (left, result) = coins.sorted.reverse.foldLeft((v, List.empty[(Int, Int)])) { (r, coin) =>
      val (left, result) = r
      (left % coin, result :+ (coin, left / coin))
    }

    println(s"Left: $left with $result")

    if (left == 0) Some(result.foldLeft(0)((r, t) => r + t._2))
    else None
  }

  def dynamic(v: Int, coins: List[Int]): Option[Int] = {
    require(v >= 0)
    require(coins.forall(_ > 0))

    def calculateBasedOnSubRoute(paths: Vector[Int], currentIndex: Int): Vector[Int] = {
      coins.foldLeft(paths){ (p, c) =>
        if (currentIndex - c < 0) p
        else {
          val subRoute = p(currentIndex - c)
          if (subRoute != Int.MaxValue && subRoute + 1 < p(currentIndex)) {
            p.updated(currentIndex, subRoute + 1)
          } else p
        }
      }
    }

    val paths = Vector.fill(v + 1)(Int.MaxValue).updated(0, 0)
    val result = paths.indices.foldLeft(paths){ (r, i) =>
      calculateBasedOnSubRoute(r, i)
    }.last
    if (result == Int.MaxValue) None
    else Some(result)
  }
}
