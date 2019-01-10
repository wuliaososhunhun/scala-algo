package compress

import scala.annotation.tailrec
import scala.collection.mutable

object HuffmanCoding {
  def main(args: Array[String]): Unit = {

    val sample = "so much words wow many compression"
    val (data, tree) = compress(sample)
    println(Bit.toString(data))

    val text = decompress(data, tree)
    println(text)
    require(sample == text)
  }

  def compress(text: String): (List[Bit], DictTree) = {
    require(text.nonEmpty)
    val tree = DictTree.build(text)
    val map = genEncMap(tree)
    val data = text.toCharArray.foldLeft(List.empty[Bit]) { (l, c) =>
      l ++ map(c)
    }
    (data, tree)
  }

  private def genEncMap(tree: DictTree): Map[Char, List[Bit]] = {

    @tailrec
    def rec(map: Map[Char, List[Bit]], unprocessed: List[(List[Bit], DictTree)]): Map[Char, List[Bit]] = {
      unprocessed match {
        case Nil => map
        case (prefix, leaf: Leaf) :: tail => rec(map + (leaf.value -> prefix), tail)
        case (prefix, branch: Branch) :: tail =>
          rec(map, (prefix :+ Zero, branch.left) :: (prefix :+ One, branch.right) :: tail)
      }
    }

    rec(Map.empty, List((List.empty, tree)))
  }

  def decompress(data: List[Bit], dict: DictTree): String = {

    @tailrec
    def decode(text: Vector[Char], data: List[Bit], currentNode: DictTree): Vector[Char] = {
      currentNode match {
        case Leaf(v, _) => decode(text :+ v, data, dict)
        case branch: Branch =>
          data match {
            case Nil => text
            case head :: tail =>
              val newNode = head match {
                case Zero => branch.left
                case One => branch.right
              }
              decode(text, tail, newNode)
          }
      }
    }

    decode(Vector.empty, data, dict).mkString("")
  }
}

sealed trait DictTree {
  val count: Int
}

object DictTree {
  implicit val ordering: Ordering[DictTree] = Ordering.by(_.count)

  def build(text: String): DictTree = {
    val counts: Map[Char, Int] = text.toCharArray.foldLeft(Map.empty[Char, Int]) { case (m, c) =>
      val oldCount = m.getOrElse(c, 0)
      m + (c -> (oldCount + 1))
    }
    require(counts.keys.size > 1)
    val leaves: mutable.PriorityQueue[DictTree] =
      mutable.PriorityQueue[DictTree](counts.map { case (char, count) => Leaf(char, count) }.toSeq: _*)(ordering.reverse)

    @tailrec
    def buildRec(queue: mutable.PriorityQueue[DictTree]): mutable.PriorityQueue[DictTree] = {
      if (queue.size == 1) queue
      else {
        val node1 = queue.dequeue()
        val node2 = queue.dequeue()
        val merged = Branch.build(node1, node2)
        queue.enqueue(merged)
        buildRec(queue)
      }
    }

    buildRec(leaves).dequeue()
  }
}

case class Leaf(value: Char, count: Int) extends DictTree

case class Branch(count: Int, left: DictTree, right: DictTree) extends DictTree

object Branch {
  def build(left: DictTree, right: DictTree): Branch = Branch(left.count + right.count, left, right)
}

sealed trait Bit {
  def toInt: Int
}

object Bit {
  def toString(data: List[Bit]): String = data.map(_.toInt.toString).mkString("")
}

case object Zero extends Bit {
  val toInt = 0
}
case object One extends Bit {
  val toInt = 1
}