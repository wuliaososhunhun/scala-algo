package structure.graph

import scala.annotation.tailrec

object DijkstraAlgo {
  def main(args: Array[String]): Unit = {
    val graph = WeightedGraph[String, Int](Set.empty)
      .addNewNode("a")
      .addNewNode("b")
      .addNewNode("c")
      .addNewNode("d")
      .addNewNode("e")
      .addEdge("a", "b", 3)
      .addEdge("a", "d", 1)
      .addEdge("b", "c", 5)
      .addEdge("b", "d", 4)
      .addEdge("b", "e", 5)
      .addEdge("c", "e", 9)
      .addEdge("d", "e", 1)


    val result = calculate(graph, "a")
    println(result.size)
    result.values.map(r => s"${r.path.mkString("->")} : ${r.totalWeight}").foreach(println)
  }

  private def calculate[T](graph: WeightedGraph[T, Int], start: T): Map[T, Result[T]] = {

    @tailrec
    def rec(resultMap: Map[T, Result[T]], unvisited: Set[T]): Map[T, Result[T]] = {
      if (unvisited.isEmpty) resultMap
      else {
        val shortest = unvisited.map(resultMap).min
        val node = graph.find(shortest.id).getOrElse(throw new IllegalStateException(s"${shortest.id} does not exist in graph"))
        val newMap = node.edges.foldLeft(resultMap) { (r, edge) =>
          val newPathResult = Result(edge.id, shortest.path :+ edge.id, Some(shortest.totalWeight.getOrElse(0) + edge.weight))
          val existPathResult = r(edge.id)
          r + (edge.id -> Ordering[Result[T]].min(newPathResult, existPathResult))
        }
        rec(newMap, unvisited - shortest.id)
      }
    }

    val nodeIds = graph.nodes.map(_.id)
    val initialMap = nodeIds.map(id => id -> Result(id, List.empty, None)).toMap +
      (start -> Result(start, List(start), Some(0)))
    rec(initialMap, nodeIds)
  }

  case class Result[T](id: T, path: List[T], totalWeight: Option[Int])

  object Result {
    implicit def ordering[T]: Ordering[Result[T]] = { (x: Result[T], y: Result[T]) =>
      Ordering[Int].compare(x.totalWeight.getOrElse(Int.MaxValue), y.totalWeight.getOrElse(Int.MaxValue))
    }
  }
}
