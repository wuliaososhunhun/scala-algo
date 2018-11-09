package book.gen

import book.gen.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}
import Generator._

import scala.util.{Failure, Success, Try}

/**
  * Author: yanyang.wang
  * Date: 06/11/2018
  */
object Test {
  def main(args: Array[String]): Unit = {
    val smallInt = choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    val sortedProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      (sorted.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists {
        case (a,b) => a > b
      }) && ns.forall(sorted.contains) && sorted.forall(ns.contains)
    }
    run(sortedProp)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def boolean: Gen[Boolean] = Gen(Generator.boolean)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Generator.nonNegativeInt.map(n => start + n % (stopExclusive - start)))
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (_, n, rng) => randomStream(as)(rng).zipWithIndex.take(n).map {
      case (a, i) => Try(if (f(a)) Passed else Falsified(a.toString, i)) match {
        case Success(r) => r
        case Failure(e) => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.iterate(g.sample.run(rng))(as => g.sample.run(as._2)).map(_._1)

  def buildMsg[A](s: A, e: Throwable): String = {
    s"""
       |test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       | ${e.getStackTrace.mkString("\n")}
     """.stripMargin
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casePerSize = (n + max - 1) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map { p =>
        Prop { (max, _, rng) =>
          p.run(max, casePerSize, rng)
        }
      }.toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, t, rng) =>
    run(max, t, rng) match {
      case Passed => p.run(max, t, rng)
      case failed => failed
    }
  }

  def ||(p: Prop): Prop = Prop { (max, t, rng) =>
    run(max, t, rng) match {
      case Falsified(e, _) => p.tag(e).run(max, t, rng)
      case failed => failed
    }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen.listOfN(i, this))

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)


  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(boolean).flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(Generator.double).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(i => forSize(i).flatMap(f))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(Gen.listOfN(_, g))
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
}