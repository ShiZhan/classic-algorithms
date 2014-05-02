case class Graph(m: Array[Array[Int]]) {
  override def toString = m.map(_.mkString(" ")).mkString("\n")
}

object GraphReachability {
  import scala.util.Random

  def main(args: Array[String]) = {
    val vertexTotal = args(0).toInt
    val m = Array.fill(vertexTotal)(Array.fill(vertexTotal)(Random.nextInt(2)))
    val g = Graph(m)

    println(g)
  }
}