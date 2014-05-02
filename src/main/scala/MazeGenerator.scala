case class Maze(size: Int) {
  import scala.util.Random
  private def wall = if (Random.nextInt(10) > 3) 1 else 0
  private val cells = Iterator.tabulate(size + 1) { r => (
    Iterator.tabulate(size + 1) { v => if (r == 0) 1 else if (v == 0 || v == size) 0 else wall },
    Iterator.tabulate(size + 1) { h => if (h == 0) 1 else if (r == 0 || r == size) 0 else wall })
  }

  override def toString =
    cells.map { case (vLine, hLine) =>
      vLine.map { case 0 => "|"; case _ => " " }.mkString("  ", " ", "\n") +
      hLine.map { case 0 => "-"; case _ => " " }.mkString(" ", "+", "+\n")
    }.toList.mkString
}

object MazeGenerator {
  def main(args: Array[String]) = {
    val m = Maze(args.head.toInt)
    println(m)
  }
}
