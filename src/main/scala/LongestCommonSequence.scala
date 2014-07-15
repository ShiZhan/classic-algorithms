object LongestCommonSequence {
  private def get(t: Array[Array[Int]], i: Int, j: Int) = if (i < 0 || j < 0) 0 else t(i)(j)

  type EqualOp[T] = (T, T) => Boolean
  def LCS[T](left: Seq[T], right: Seq[T])(e: EqualOp[T]) = {
    val t = Array.fill(left.length, right.length)(0)
    for ((l, i) <- left.zipWithIndex; (r, j) <- right.zipWithIndex)
      t(i)(j) = if (e(l, r)) get(t, i - 1, j - 1) + 1 else get(t, i - 1, j) max get(t, i, j - 1)

    t.map(_.mkString(" ")).foreach(println)

    val lcsLen = t(left.length - 1)(right.length - 1)
    val lcs = Array.fill(lcsLen)((0, 0))
    var (i, j) = (left.length - 1, right.length - 1)
    while (i >= 0 && j >= 0) {
      if (e(left(i), right(j))) {
        lcs(t(i)(j) - 1) = (i, j)
        i -= 1
        j -= 1
      } else if (get(t, i - 1, j) > get(t, i, j - 1)) i -= 1 else j -= 1
    }
    lcs
  }

  implicit class LCSop[T](left: Seq[T]) {
    def <=>(right: Seq[T])(e: EqualOp[T]) = LCS[T](left, right)(e)
  }

  def main(args: Array[String]) = {
    val seq1 = args(0).toSeq
    val seq2 = args(1).toSeq
    (seq1 <=> seq2) { _ == _ }.foreach(println)
  }
}