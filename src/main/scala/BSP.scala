class BspGraph(m: Array[Array[Int]]) extends Graph(m) {
  import scala.actors.Actor
  import scala.actors.Actor._

  def shortestPath(target: Int) = {
    println("TODO ...")
  }
}

object BspShortestPath {
  import scala.util.Random

  def main(args: Array[String]) = {
    val vertexTotal = args(0).toInt
    val m = Array.fill(vertexTotal, vertexTotal)(Random.nextInt(2))
    val g = new Graph(m)
    println(g)
  }
}