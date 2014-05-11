case class Maze(height: Int, width: Int) {
  import scala.util.Random

  private def wall = if (Random.nextInt(10) > 3) 1 else 0
  private val cells = Array.tabulate(height + 1) { r =>
    Array.tabulate(width + 1) { c =>
      val v = if (r == 0) 1 else if (c == 0 || c == width) 0 else wall
      val h = if (c == 0) 1 else if (r == 0 || r == height) 0 else wall
      (v, h)
    }
  }

  override def toString = cells.map {
    case row =>
      val (v, h) = row.unzip
      val vLine = v.map { case 0 => "|"; case _ => " " }.mkString("  ", " ", "\n")
      val hLine = h.map { case 0 => "-"; case _ => " " }.mkString(" ", "+", "+\n")
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
        if (v == 1) { g(thisCell)(eastCell) = 1; g(eastCell)(thisCell) = 1 }
        if (h == 1) { g(thisCell)(southCell) = 1; g(southCell)(thisCell) = 1 }
      }
    g
  }
}

object MazeGenerator {
  def main(args: Array[String]) = {
    val m = Maze(args(0).toInt, args(1).toInt)
    println(m)
    val g = Graph(m.toGraph)
    println(g); println(g.reachable(0, args(0).toInt * args(1).toInt - 1))
  }
}
