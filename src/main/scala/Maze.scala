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
    for (i <- (0 to (height - 1)))
      for (j <- (0 to (width - 1))) {
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
  import scala.util.Random

  private def wall = if (Random.nextInt(10) > 3) 0 else 1

  for (r <- 0 to height) { hMat(r)(0) = 0; if (r > 0 && r < height) hMat(r)(width) = wall }
  for (c <- 0 to width) { vMat(0)(c) = 0; if (c > 0 && c < width) vMat(height)(c) = wall }
  for (r <- 1 to height - 1; c <- 1 to width - 1) { vMat(r)(c) = wall; hMat(r)(c) = wall }
}

case class ConnectedMaze(height: Int, width: Int) extends Maze(height, width) {
  import scala.util.Random

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
      case 0 => if (c < width) if (isVisited(east)) (0, 0) else { vMat(r)(c) = 0; east } else (0, 0)
      case 1 => if (r < height) if (isVisited(south)) (0, 0) else { hMat(r)(c) = 0; south } else (0, 0)
      case 2 => if (c > 1) if (isVisited(west)) (0, 0) else { vMat(r)(c - 1) = 0; west } else (0, 0)
      case 3 => if (r > 1) if (isVisited(north)) (0, 0) else { hMat(r - 1)(c) = 0; north } else (0, 0)
      case _ => (0, 0)
    }
  }

  private def dig(r: Int, c: Int): Unit = {
    visited(r)(c) = true; print("*")
    val anyDirection = Random.nextInt(4)
    (0 to 3).map { d => (d + anyDirection) % 4 } foreach { d =>
      val next = (r, c).to(d)
      if (next != (0, 0)) { val (r1, c1) = next; dig(r1, c1) }
    }
  }

  dig(height / 2, width / 2)
}

object SimpleMazeGenerator {
  def main(args: Array[String]) = {
    val m = SimpleMaze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph)
    println(g)
    println(g.reachable(0, args(0).toInt * args(1).toInt - 1))
  }
}

object ConnectedMazeGenerator {
  def main(args: Array[String]) = {
    val m = ConnectedMaze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph)
    println(g.reachable(0, args(0).toInt * args(1).toInt - 1))
  }
}
