case class Graph(m: Array[Array[Int]]) {
  val vertexTotal = m.length

  assert(vertexTotal == m.head.length)

  override def toString = m.map(_.mkString(" ")).mkString("\n")

  private def _reachable(from: Int, to: Int, visited: Array[Boolean]): Boolean = {
    visited(from) = true
    if (from == to) true
    else
      m(from).zipWithIndex.exists {
        case (edge, vertex) =>
          !visited(vertex) && edge > 0 && _reachable(vertex, to, visited)
      }
  }

  def reachable(from: Int, to: Int) =
    _reachable(from, to, Array.fill(vertexTotal)(false))
}

object GraphReachability {
  import scala.util.Random

  def main(args: Array[String]) = {
    val vertexTotal = args(0).toInt
    val m = Array.fill(vertexTotal, vertexTotal)(Random.nextInt(2))
    val g = Graph(m)
    println(g)
    println(g.reachable(0, vertexTotal - 1))
  }
}
