object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val skipTable: Map[Char, Int] = getSkipNums(pattern)  //文字をキー、その文字が出現した際にずらす数を値に持つ連想配列
    val lastIndex = text.length - pattern.length
    var matchIndexes = Seq[Int]()
    var i = 0

    while (i <= lastIndex) {
      val partText = text.slice(i, i + pattern.length)
      val (isMatch, shift) = matchTextAndGetNumToShift(partText, pattern, skipTable)  //isMatch: Boolean, shift: Int
      if (isMatch) matchIndexes = matchIndexes :+ i
      i = i + shift
    }

    matchIndexes
  }

  def getSkipNums(pattern: Seq[Char]): Map[Char, Int] = {
    var skipTable: Map[Char, Int] = Map()

    for (i <- 0 until pattern.length) {
      skipTable += (pattern(i) -> (pattern.length - i - 1))
    }

    skipTable
  }

  def matchTextAndGetNumToShift(partText: Seq[Char], pattern: Seq[Char], skipTable: Map[Char, Int]): (Boolean, Int) = {
    var isMatch = false
    var shift = pattern.length

    for (i <- pattern.length - 1 to 0 by -1) {
      if (partText(i) != pattern(i)) {
        shift = skipTable.getOrElse(partText(i), shift)
        return (isMatch, shift)
      }
    }
    isMatch = true

    (isMatch, shift)
  }

  println(s"出現場所: ${matchIndexes}")
}