class BspGraph(m: Array[Array[Int]]) extends Graph(m) {
  import scala.actors.Actor
  import scala.actors.Actor._

  def in(index: Int) = m.map(_(index)).zipWithIndex.filter(_._1 != 0)
  def out(index: Int) = m(index).zipWithIndex.filter(_._1 != 0)

  sealed abstract class Messages

  case class UPDATE(value: Int) extends Messages

  case class STOP extends Messages

  class Vertex(index: Int) extends Actor {
    var value = Int.MaxValue
    def act() = {
      loop {
        react {
          case UPDATE(newValue) if (newValue < value) =>
            value = newValue
            for ((length, next) <- in(index)) {
              println(s"$index->$next:$value+$length")
              vertices(next) ! UPDATE(value + length)
            }
          case STOP => exit
        }
      }
    }
  }

  val vertices = Array.tabulate(vertexTotal)(new Vertex(_))

  def shortestPath(target: Int) = {
    vertices.foreach(_.start)
    vertices(target) ! UPDATE(0)
    for (line <- io.Source.stdin.getLines) line match { case "exit" => vertices.foreach(_ ! STOP) }
  }
}

object BspShortestPath {
  import scala.util.Random

  def main(args: Array[String]) = {
    val vertexTotal = args.head.toInt
    val m = Array.fill(vertexTotal, vertexTotal)(Random.nextInt(2))
    val g = new BspGraph(m)
    println(g)
    g.shortestPath(vertexTotal - 1)
  }
}