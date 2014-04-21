object TowersOfHanoi {
  def move(n: Int, from: String, to: String, via: String) : Unit = {
    if (n == 1) {
      Console.println("Move disk from " + from + " pole to " + to + " pole ")
    } else {
      move(n - 1, from, via, to)
      move(1, from, to, via)
      move(n - 1, via, to, from)
    }
  }

  def main(args: Array[String]) = {
    move(args.head.toInt, "Left", "Right", "Center")
  }
}
