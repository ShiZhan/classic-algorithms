object LongestCommonSequence {
  case class CompareTable(lengthL: Int, lengthR: Int) {
    private val t = Array.fill(lengthL)(Array.fill(lengthR)(0))
    def set(i: Int, j: Int, value: Int) = { t(i)(j) = value }
    def get(i: Int, j: Int) = if (i < 0 || j < 0) 0 else t(i)(j)
  }

  implicit class LCSop[T](left: Seq[T]) {
    def <=>(right: Seq[T]) = {
      val t = CompareTable(left.length, right.length)
      for ((l, i) <- left.zipWithIndex)
        for ((r, j) <- right.zipWithIndex)
          t.set(i, j, if (l == r) t.get(i - 1, j - 1) + 1 else t.get(i - 1, j) max t.get(i, j - 1))
      t.get(left.length - 1, right.length - 1)
    }
  }

  def main(args: Array[String]) = {
    val seq1 = args(0).toSeq
    val seq2 = args(1).toSeq
    println(seq1 <=> seq2)
  }
}