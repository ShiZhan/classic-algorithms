object Entropy {
  import scala.math._
  import scala.util.Random

  def entropy(v: String) = {
    v.groupBy(a => a).values
      .map(i => i.length.toDouble / v.length)
      .map(p => -p * log10(p) / log10(2))
      .sum
  }

  def randomStr(length: Int) = {
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ Array('\n', ' ')
    Iterator.continually(chars(Random.nextInt(chars.length))).take(length)
  }

  def main(args: Array[String]) = {
    val information = if (args.length > 0) args.mkString(" ") else randomStr(100).mkString
    println(information)
    println(entropy(information))
  }
}
