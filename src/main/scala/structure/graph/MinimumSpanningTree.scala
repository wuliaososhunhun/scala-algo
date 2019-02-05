package structure.graph

import scala.annotation.tailrec

object MinimumSpanningTree {
  def main(args: Array[String]): Unit = {
    val graph = UnWeightedGraph[String](Set.empty)
      .addNewNode("a")
      .addNewNode("b")
      .addNewNode("c")
      .addNewNode("d")
      .addNewNode("e")
      .addNewNode("f")
      .addNewNode("g")
      .addNewNode("h")
      .addNewNode("i")
      .addEdge("a", "b")
      .addEdge("a", "h")
      .addEdge("b", "a")
      .addEdge("b", "c")
      .addEdge("b", "h")
      .addEdge("c", "b")
      .addEdge("c", "d")
      .addEdge("c", "f")
      .addEdge("c", "i")
      .addEdge("d", "c")
      .addEdge("d", "e")
      .addEdge("d", "f")
      .addEdge("e", "d")
      .addEdge("e", "f")
      .addEdge("f", "c")
      .addEdge("f", "d")
      .addEdge("f", "e")
      .addEdge("f", "g")
      .addEdge("g", "f")
      .addEdge("g", "h")
      .addEdge("g", "i")
      .addEdge("h", "a")
      .addEdge("h", "b")
      .addEdge("h", "g")
      .addEdge("h", "i")
      .addEdge("i", "c")
      .addEdge("i", "g")
      .addEdge("i", "h")

    val result = calculate(graph, "a")
    println(result.nodes.toList.sortBy(_.id))
  }

  private def calculate[T](graph: UnWeightedGraph[T], start: T): UnWeightedGraph[T] = {

    @tailrec
    def rec(graph: UnWeightedGraph[T], pendingNodes: List[(Option[T], T)], visited: Set[T]): UnWeightedGraph[T] = {
      pendingNodes match {
        case Nil => graph
        case head :: tail if visited.contains(head._2) =>
          rec(graph, tail, visited)
        case head :: tail =>
          val (pre, current) = head
          graph.find(current) match {
            case None => throw new IllegalStateException(s"Node $head does not exist in graph")
            case Some(node) =>
              val updatedVisited = visited + node.id
              val updatedGraph = (node.neighbours.intersect(updatedVisited) -- pre).foldLeft(graph) { (g, n) =>
                g.removeEdge(node.id, n)
              }
              val updatedPending = tail ++ (node.neighbours -- visited).map((Some(node.id), _))
              rec(updatedGraph, updatedPending, updatedVisited)
          }
      }
    }

    rec(graph, List((None, start)), Set.empty)
  }
}
