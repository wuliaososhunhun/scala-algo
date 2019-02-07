package structure.graph

import scala.annotation.tailrec

object WeightedMinimumSpanningTree {
  def main(args: Array[String]): Unit = {
    val graph = WeightedGraph[String, Int](Set.empty)
      .addNewNode("a")
      .addNewNode("b")
      .addNewNode("c")
      .addNewNode("d")
      .addNewNode("e")
      .addNewNode("f")
      .addEdge("a", "b", 4)
      .addEdge("a", "c", 4)
      .addEdge("b", "c", 2)
      .addEdge("c", "d", 3)
      .addEdge("c", "e", 4)
      .addEdge("c", "f", 2)
      .addEdge("d", "e", 3)
      .addEdge("d", "f", 3)


    val result = calculate(graph)
    println(result.nodes.toList.sortBy(_.id))
  }

  private def calculate[T, W](graph: WeightedGraph[T, W])(implicit order: Ordering[W]): WeightedGraph[T, W] = {

    val allNodes = graph.nodes.map(_.id)
    val ordered = graph.nodes.toList.flatMap(node => node.edges.toList.map((node.id, _))).sortBy(_._2.weight)
    @tailrec
    def rec(graph: WeightedGraph[T, W], pendingEdges: List[(T, Edge[T, W])], groups: Set[Set[T]]): WeightedGraph[T, W] = {
      if (groups.size == 1) graph
      else {
        pendingEdges match {
          case Nil => graph
          case head :: tail if groups.exists(group => group.contains(head._1) && group.contains(head._2.id)) =>
            rec(graph, tail, groups)
          case head :: tail =>
            val (id1, edge) = head
            val updatedGraph = graph.addEdge(id1, edge.id, edge.weight)
            val (affected, others) = groups.partition(p => p.contains(id1) || p.contains(edge.id))
            rec(updatedGraph, tail, others + affected.flatten)
        }
      }
    }
    val rawGraph = allNodes.foldLeft(WeightedGraph[T, W](Set.empty)) { (g, n) =>
      g.addNewNode(n)
    }
    rec(rawGraph, ordered, allNodes.map(Set(_)))
  }
}
