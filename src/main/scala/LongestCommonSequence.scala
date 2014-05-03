object LongestCommonSequence {
  implicit class LCSop[T](left: Seq[T]) {
    private def get(t: Array[Array[Int]], i: Int, j: Int) = if (i < 0 || j < 0) 0 else t(i)(j)
    def <=>(right: Seq[T]) = {
      val t = Array.fill(left.length)(Array.fill(right.length)(0))
      val p = Array.fill(left.length)(Array.fill(right.length)(0))
      for ((l, i) <- left.zipWithIndex)
        for ((r, j) <- right.zipWithIndex) {
          t(i)(j) = if (l == r) get(t, i - 1, j - 1) + 1 else get(t, i - 1, j) max get(t, i, j - 1)
          p(i)(j) = if (l == r) 1 else 0
        }
      t(left.length - 1)(right.length - 1)
    }
  }

  def main(args: Array[String]) = {
    val seq1 = args(0).toSeq
    val seq2 = args(1).toSeq
    println(seq1 <=> seq2)
  }
}