class Maze(height: Int, width: Int) {
  val vMat = Array.fill(height + 1, width + 1)(1)
  val hMat = Array.fill(height + 1, width + 1)(1)

  override def toString = (0 to height).map { r =>
    val vLine = vMat(r).map { case 1 => "|"; case _ => " " }.mkString("  ", " ", "\n")
    val hLine = hMat(r).map { case 1 => "-"; case _ => " " }.mkString(" ", "+", "+\n")
    vLine + hLine
  }.mkString

  def toGraph = {
    val cellTotal = height * width
    val g = Array.fill(cellTotal, cellTotal)(0)
    for (i <- (0 to height - 1); j <- (0 to width - 1)) {
      val thisCell = i * width + j
      val southCell = (i + 1) % height * width + j
      val eastCell = i * width + ((j + 1) % width)
      if (vMat(i + 1)(j + 1) == 0) { g(thisCell)(eastCell) = 1; g(eastCell)(thisCell) = 1 }
      if (hMat(i + 1)(j + 1) == 0) { g(thisCell)(southCell) = 1; g(southCell)(thisCell) = 1 }
    }
    g
  }
}

case class SimpleMaze(height: Int, width: Int) extends Maze(height, width) {
  private def wall = if (util.Random.nextInt(10) > 3) 0 else 1

  for (r <- 0 to height) hMat(r)(0) = 0; for (r <- 1 to height - 1) hMat(r)(width) = wall
  for (c <- 0 to width) vMat(0)(c) = 0; for (c <- 1 to width - 1) vMat(height)(c) = wall
  for (r <- 1 to height - 1; c <- 1 to width - 1) { vMat(r)(c) = wall; hMat(r)(c) = wall }
}

case class ConnectedMaze(height: Int, width: Int) extends Maze(height, width) {
  private val visited = Array.fill(height + 1, width + 1)(false)
  private def isVisited(p: (Int, Int)) = visited(p._1)(p._2)

  (0 to height).foreach { hMat(_)(0) = 0 }
  (0 to width).foreach { vMat(0)(_) = 0 }

  implicit class Position(p: (Int, Int)) {
    val (r, c) = p
    val east = (r, c + 1)
    val south = (r + 1, c)
    val west = (r, c - 1)
    val north = (r - 1, c)
    def to(direction: Int) = direction match {
      case 0 => if (c < width) if (isVisited(east)) None else { vMat(r)(c) = 0; Some(east) } else None
      case 1 => if (r < height) if (isVisited(south)) None else { hMat(r)(c) = 0; Some(south) } else None
      case 2 => if (c > 1) if (isVisited(west)) None else { vMat(r)(c - 1) = 0; Some(west) } else None
      case 3 => if (r > 1) if (isVisited(north)) None else { hMat(r - 1)(c) = 0; Some(north) } else None
      case _ => None
    }
  }

  private def dig(r: Int, c: Int): Unit = {
    visited(r)(c) = true
    val anyDirection = util.Random.nextInt(4)
    (0 to 3).map { d => (d + anyDirection) % 4 } foreach { d =>
      (r, c).to(d) match { case Some((r1, c1)) => dig(r1, c1); case None => }
    }
  }

  dig(height / 2, width / 2)
}

object SimpleMazeGenerator {
  def main(args: Array[String]) = {
    val m = SimpleMaze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph); val vertexTotal = args(0).toInt * args(1).toInt
    println(g)
    println(g.reachable(0, vertexTotal - 1, Array.fill(vertexTotal)(false)))
  }
}

object ConnectedMazeGenerator {
  def main(args: Array[String]) = {
    val m = ConnectedMaze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph); val vertexTotal = args(0).toInt * args(1).toInt
    println(g.dijkstra(List((0, List(0))), vertexTotal - 1, Set[Int]()))
  }
}
