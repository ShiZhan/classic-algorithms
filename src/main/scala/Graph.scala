class Graph(m: Array[Array[Int]]) {
  val vertexTotal = m.length

  assert(vertexTotal == m.head.length)

  override def toString = m.map(_.mkString(" ")).mkString("\n")

  def reachable(from: Int, to: Int, visited: Array[Boolean]): Boolean = {
    visited(from) = true
    if (from == to) true
    else
      m(from).zipWithIndex.exists {
        case (edge, vertex) =>
          !visited(vertex) && edge > 0 && reachable(vertex, to, visited)
      }
  }

  type Path = (Int, List[Int])

  def dijkstra(from: List[Path], to: Int, visited: Set[Int]): Path = from match {
    case (distance, path) :: candidates => path match {
      case current :: path_rest =>
        if (current == to) (distance, path.reverse)
        else {
          val paths = m(current).toList.zipWithIndex.flatMap {
            case (e, v) =>
              if (e > 0 && !visited.contains(v)) List((distance + e, v :: path)) else Nil
          }
          val sorted_candidates = (paths ++ candidates).sortBy(_._1)
          dijkstra(sorted_candidates, to, visited + current)
        }
      case Nil => (0, List())
    }
    case Nil => (0, List())
  }
}

object GraphTraverse {
  import scala.util.Random

  def main(args: Array[String]) = {
    val vertexTotal = args(0).toInt
    val m = Array.fill(vertexTotal, vertexTotal)(Random.nextInt(2))
    val g = new Graph(m)
    println(g)
    println(g.reachable(0, vertexTotal - 1, Array.fill(vertexTotal)(false)))
    println(g.dijkstra(List((0, List(0))), vertexTotal - 1, Set[Int]()))
  }
}
