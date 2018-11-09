package book.gen

/**
  * Author: yanyang.wang
  * Date: 21/10/2018
  */
object Generator {
  def main(args: Array[String]): Unit = {
    val one = rollDie.run(SimpleRNG(5))._1
    println(one)
  }

  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  def nonNegativeInt: Rand[Int] = int.map(i => if (i < 0) -(i + 1) else i)

  def nonNegativeEven: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

  val boolean: Rand[Boolean] = int.map(_ % 2 == 0)
  val intInt: Rand[(Int, Int)] = both(int, int)
  val double: Rand[Double] = nonNegativeInt.map(_.toDouble / (Int.MaxValue + 1))


  val intDouble: Rand[(Int, Double)] = both(int, double)

  val doubleInt: Rand[(Double, Int)] = both(double, int)

  def double3: Rand[(Double, Double, Double)] = {
    for {
      d1 <- double
      d2 <- double
      d3 <- double
    } yield (d1, d2, d3)
  }

  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =  nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
  }

  def rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    g(a).run(s2)
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft(unit[S, List[A]](List.empty)){ (acc, f) =>
      acc.map2(f)(_ :+ _)
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}