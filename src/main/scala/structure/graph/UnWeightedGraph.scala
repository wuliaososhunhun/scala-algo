package structure.graph

case class UnWeightedGraph[T](nodes: Set[Node[T]]) {
  def find(id: T): Option[Node[T]] = nodes.find(_.id == id)

  def exits(id: T): Boolean = find(id).isDefined

  def updateNodeOption(node: Option[Node[T]]): UnWeightedGraph[T] = {
    node match {
      case None => this
      case Some(n) => updateNode(n)
    }
  }

  def updateNode(node: Node[T]): UnWeightedGraph[T] =
    copy(nodes = nodes.filterNot(_.id == node.id) + node)

  def addNewNode(id: T): UnWeightedGraph[T] =
    if (exits(id)) {
      println(s"add existing node when add new node $id")
      this
    } else {
      UnWeightedGraph(nodes + Node(id, Set.empty))
    }

  def removeNode(id: T): UnWeightedGraph[T] = {
    val filtered = nodes.filterNot(_.id == id)
    val cleaned = filtered.map(_.disconnect(id))
    UnWeightedGraph(cleaned)
  }

  def addEdge(id1: T, id2: T): UnWeightedGraph[T] = {
    val result = for {
      node1 <- find(id1)
      node2 <- find(id2)
      if id1 != id2
    } yield {
      Node.connect(node1, node2)
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

  def removeEdge(id1: T, id2: T): UnWeightedGraph[T] = {
    updateNodeOption(
      find(id1).map(_.disconnect(id2))
    ).updateNodeOption(
      find(id2).map(_.disconnect(id1))
    )
  }
}

case class Node[T](id: T, neighbours: Set[T]) {
  require(!neighbours.contains(id), s"$id cannot have neighbour $neighbours")

  def connect(target: T): Either[String, Node[T]] = {
    if (id == target) {
      Left(s"$target cannot not connect to itself")
    } else {
      Right(copy(neighbours = neighbours + target))
    }
  }

  def disconnect(id: T): Node[T] = copy(neighbours = neighbours - id)
}

object Node {
  def connect[T](node1: Node[T], node2: Node[T]): Either[String, (Node[T], Node[T])] = {
    for {
      update1 <- node1.connect(node2.id)
      update2 <- node2.connect(node1.id)
    } yield (update1, update2)
  }
}

