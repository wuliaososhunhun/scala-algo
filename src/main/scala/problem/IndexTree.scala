package problem

object IndexTree {
  def main(args: Array[String]): Unit = {
    val root: Tree[String] = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    println(index(root))
  }

  def index[A](root: Tree[A]): Tree[(A, Int)] = {

    def rec(t: Tree[A], id: Int): (Tree[(A, Int)], Int) = {
      t match {
        case Branch(left, right) =>
          val (newLeft, newId) = rec(left, id)
          val (newRight, newNewId) = rec(right, newId)
          (Branch(newLeft, newRight), newNewId)
        case Leaf(v) =>
          (Leaf((v, id)), id + 1)
      }
    }

    rec(root, 0)._1
  }
}

sealed trait Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]