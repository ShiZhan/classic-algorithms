class Maze(height: Int, width: Int) {
  val cells = Array.fill(height + 1, width + 1)((1, 1))

  override def toString = cells.map { row =>
    val (v, h) = row.unzip
    val vLine = v.map { case 1 => "|"; case _ => " " }.mkString("  ", " ", "\n")
    val hLine = h.map { case 1 => "-"; case _ => " " }.mkString(" ", "+", "+\n")
    vLine + hLine
  }.mkString

  def toGraph = {
    val cellTotal = height * width
    val g = Array.fill(cellTotal, cellTotal)(0)
    for (i <- (0 to (height - 1)))
      for (j <- (0 to (width - 1))) {
        val (v, h) = cells(i + 1)(j + 1)
        val thisCell = i * width + j
        val southCell = (i + 1) % height * width + j
        val eastCell = i * width + ((j + 1) % width)
        if (v == 0) { g(thisCell)(eastCell) = 1; g(eastCell)(thisCell) = 1 }
        if (h == 0) { g(thisCell)(southCell) = 1; g(southCell)(thisCell) = 1 }
      }
    g
  }
}

case class SimpleMaze(height: Int, width: Int) extends Maze(height, width) {
  import scala.util.Random

  private def wall = if (Random.nextInt(10) > 3) 1 else 0

  (0 to height).foreach { r =>
    (0 to width).foreach { c =>
      val v = if (r == 0) 0 else if (c == 0 || c == width) 1 else wall
      val h = if (c == 0) 0 else if (r == 0 || r == height) 1 else wall
      cells(r)(c) = (v, h)
    }
  }
}

case class ConnectedMaze(height: Int, width: Int) extends Maze(height, width) {
  import scala.util.Random

  cells(0)(0) = (0, 0)
  (1 to height).foreach { cells(_)(0) = (1, 0) }
  (1 to width).foreach { cells(0)(_) = (0, 1) }

  implicit class Position(p: (Int, Int)) {
    val (r, c) = p
    private def dig(to: Int) = {
    }
  }
}

object SimpleMazeGenerator {
  def main(args: Array[String]) = {
    val m = SimpleMaze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph)
    println(g); println(g.reachable(0, args(0).toInt * args(1).toInt - 1))
  }
}

object ConnectedMazeGenerator {
  def main(args: Array[String]) = {
    val m = ConnectedMaze(args(0).toInt, args(1).toInt)
    println(m)
  }
}
