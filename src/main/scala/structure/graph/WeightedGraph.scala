package structure.graph

import scala.math.Ordering

case class WeightedGraph[T, W](nodes: Set[WeightNode[T, W]])(implicit order: Ordering[W]) {
  def find(id: T): Option[WeightNode[T, W]] = nodes.find(_.id == id)

  def exits(id: T): Boolean = find(id).isDefined

  def updateNodeOption(node: Option[WeightNode[T, W]]): WeightedGraph[T, W] = {
    node match {
      case None => this
      case Some(n) => updateNode(n)
    }
  }

  def updateNode(node: WeightNode[T, W]): WeightedGraph[T, W] =
    copy(nodes = nodes.filterNot(_.id == node.id) + node)

  def addNewNode(id: T): WeightedGraph[T, W] =
    if (exits(id)) {
      println(s"add existing node when add new node $id")
      this
    } else {
      WeightedGraph(nodes + WeightNode(id, Set.empty[Edge[T, W]]))
    }

  def removeNode(id: T): WeightedGraph[T, W] = {
    val filtered = nodes.filterNot(_.id == id)
    val cleaned = filtered.map(_.disconnect(id))
    WeightedGraph(cleaned)
  }

  def addEdge(id1: T, id2: T, weight: W): WeightedGraph[T, W] = {
    val result = for {
      node1 <- find(id1)
      node2 <- find(id2)
      if id1 != id2
    } yield {
      WeightNode.connect(node1, node2, weight)
    }
    result match {
      case None =>
        println(s"$id1 or $id2 does not exist in graph or they are exactly same ")
        this
      case Some(Left(error)) =>
        println(error)
        this
      case Some(Right((n1, n2))) =>
        updateNode(n1).updateNode(n2)
    }
  }

  def removeEdge(id1: T, id2: T): WeightedGraph[T, W] = {
    updateNodeOption(
      find(id1).map(_.disconnect(id2))
    ).updateNodeOption(
      find(id2).map(_.disconnect(id1))
    )
  }
}

case class WeightNode[T,  W](id: T, edges: Set[Edge[T, W]])(implicit order: Ordering[W]) {
  require(!edges.exists(_.id == id), s"$id cannot have neighbour $edges")

  def connect(target: T, weight: W): Either[String, WeightNode[T, W]] = {
    if (id == target) {
      Left(s"$target cannot not connect to itself")
    } else {
      Right(copy(edges = edges + Edge(target, weight)))
    }
  }

  def disconnect(id: T): WeightNode[T, W] = copy(edges = edges.filterNot(_.id == id))
}

object WeightNode {
  def connect[T, W](node1: WeightNode[T, W], node2: WeightNode[T, W], weight: W): Either[String, (WeightNode[T, W], WeightNode[T, W])] = {
    for {
      update1 <- node1.connect(node2.id, weight)
      update2 <- node2.connect(node1.id, weight)
    } yield (update1, update2)
  }
}

final case class Edge[T, W](id: T, weight: W)(implicit order: Ordering[W])

