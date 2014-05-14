object Entropy {
  import scala.math._
  import scala.util.Random

  def entropy[T](v: Seq[T]) = {
    v.groupBy(a => a).values
      .map(i => i.length.toDouble / v.length)
      .map(p => -p * log10(p) / log10(2))
      .sum
  }

  def randomStr(length: Int) = Iterator.continually(Random.nextPrintableChar).take(length)

  def main(args: Array[String]) = {
    val information = if (args.length > 0) args.mkString(" ") else randomStr(100).mkString
    println(information)
    println(entropy(information))
  }
}
