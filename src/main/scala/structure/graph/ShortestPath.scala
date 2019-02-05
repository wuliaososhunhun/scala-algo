package structure.graph

import scala.annotation.tailrec

object ShortestPath {
  def main(args: Array[String]): Unit = {
    val graph = ('A' to 'H').foldLeft(UnWeightedGraph[Char](Set.empty))(_.addNewNode(_))
      .addEdge('A', 'B')
      .addEdge('A', 'C')
      .addEdge('B', 'D')
      .addEdge('B', 'E')
      .addEdge('C', 'F')
      .addEdge('C', 'G')
      .addEdge('E', 'F')
      .addEdge('E', 'H')
      .addEdge('F', 'G')

    println(find(graph, 'A', 'F').mkString(" -> "))
  }

  private def find[T](graph: UnWeightedGraph[T], start: T, end: T): List[T] = {

    @tailrec
    def rec(unprocessed: List[(List[T], T)], traveled: Set[T]): List[T] = {
      unprocessed match {
        case Nil => List.empty
        case head :: tail =>
          val (pre, current) = head
          graph.find(current) match {
            case None => List.empty
            case Some(node) if traveled.contains(node.id) =>
              rec(tail, traveled)
            case Some(node) if node.id == end => pre :+ current
            case Some(node) =>
              val newSearch = (node.neighbours -- traveled).map((pre :+ current,  _)).toList
              rec(tail ++ newSearch, traveled + current)
          }
      }
    }

    rec(List((Nil, start)), Set.empty)
  }
}
