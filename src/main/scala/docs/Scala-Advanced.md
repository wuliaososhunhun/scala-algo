# Scala 2.12
- Java SE 8 or later versions only without backwards compatible e.g. crossScalaVersions in sbt
- Macro is deprecated or not supported

# Implicit scope
- Locally declared implicits
- Imported implicits
- Outer scope (ex- a class for a method)
- Inheritance
- Package object
- Implicit scope like companion object

# currying
currying is the technique of translating the evaluation of a function that takes multiple arguments into evaluating a sequence of functions, each with a single argument.

e.g f(a, b) => f(a)(b)

# inner class
In Scala, an inner class is associated with the outer class’ object. In Java, however, an inner class is associated with the outer class; the inner class is a member of the outer class.

# Functor
A type of constructor F[_] with map method `def map(f: A => B): F[B]`

# Monoid and Semigroup (combine only)
- combine(x: A, y: A): A
- empty: A 

# Applicative
```
trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] (Apply)

  def pure[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
}
```
Usage:
```
val f1 = Future(3)
val f2 = Future(4)
val f3 = Future(5)
val calculate = (a: Int) => (b: Int) => (c: Int) => a + b + c

f1.ap(f2.ap(f3.ap(pure(calculate)))) instead of for comprehension

```
sequence / traverse

# Monad
define with flatMap and pure or Applicative with flatten
- identity rule ma.flatMap(pure) == ma
- associate rule ma.flatMap(faMb).flatMap(fbMc) == ma.flatMap(a => faMb(a).flatMap(fbMc))

It is a abstraction to compose/link different function. All things are set of something and code is try to transform from 1 set to the other. The category 
theory try to abstract all interactions between sets in a mathmatcial way (you can call them patterns) so it should include all the changes in your code. Based
 on that, you can write your code based on these patterns which can solve some issue with less boiler plates. e.g .A-> B, A -> F[B], F[A] -> F[B]
 