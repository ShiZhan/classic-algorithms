object LongestCommonSequence {
  implicit class LCSop[T](left: Seq[T]) {
    private def get(t: Array[Array[Int]], i: Int, j: Int) = if (i < 0 || j < 0) 0 else t(i)(j)

    def <=>(right: Seq[T]) = {
      val t = Array.fill(left.length, right.length)(0)
      for ((l, i) <- left.zipWithIndex)
        for ((r, j) <- right.zipWithIndex)
          t(i)(j) = if (l == r) get(t, i - 1, j - 1) + 1 else get(t, i - 1, j) max get(t, i, j - 1)

      println(t.map(_.mkString(" ")).mkString("\n"))
      val lcsLen = t(left.length - 1)(right.length - 1)
      val lcs = Array.fill(lcsLen)((0, 0))
      var (i, j) = (left.length - 1, right.length - 1)
      while (i >= 0 && j >= 0) {
        if (left(i) == right(j)) {
          lcs(t(i)(j) - 1) = (i, j)
          i -= 1
          j -= 1
        } else if (get(t, i - 1, j) > get(t, i, j - 1)) i -= 1 else j -= 1
      }
      lcs
    }
  }

  def main(args: Array[String]) = {
    val seq1 = args(0).toSeq
    val seq2 = args(1).toSeq
    (seq1 <=> seq2).foreach(println)
  }
}