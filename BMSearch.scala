object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println("skipTable: " + skipTable)
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0

    while (i < text.length -1) {
      val partText = text.slice(i, i + pattern.length)
      println("partText:" + partText)
      val patternLastIndex = pattern.length - 1

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '_'
      var matchPosition = 0

      while (j >= 0 && isMatch) {
        if (j > partText.length - 1) {
          isMatch = false
        } else {
          matchChar = partText(j)
          if (matchChar != pattern(j)) {
            isMatch = false
            matchPosition = (patternLastIndex - j)
          }
        }
        j = j - 1
      }

      if (isMatch) matchIndexes = matchIndexes :+ i

      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (skipCount <= 0) skipCount = 1
      println("skipCount: " + skipCount)
      i = i + skipCount
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
