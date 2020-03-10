object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  // 非マッチ時のスキップ数をテーブルにしておく
  val skipTable = pattern.map(s => (s -> pattern.reverse.indexOf(s))).toMap
  println("skipTable: " + skipTable)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)
      println("partText: " + partText)

      val tmp = getMatchInfo(partText, pattern)
      val isMatch = tmp._1
      val matchChara = tmp._2
      val matchPosition = tmp._3
      if (isMatch) {
        matchIndexes = matchIndexes :+ i
      }

      var skipCount =
        skipTable.getOrElse(matchChara, pattern.length) - matchPosition
      if (skipCount <= 0) skipCount = 1
      println("skipCount: " + skipCount)

      i = i + skipCount
    }

    matchIndexes
  }

  def getMatchInfo(
      textPart: Seq[Char],
      pattern: Seq[Char]
  ): (Boolean, Char, Int) = {
    val patternLastIndex = pattern.length - 1
    var isMatch = true
    var j = patternLastIndex
    var matchChara = '_'
    var matchPosition = 0

    while (j >= 0 && isMatch) {
      if (j > textPart.length - 1) {
        // 切り出したテキストが短い場合 false
        isMatch = false
      } else {
        matchChara = textPart(j)
        if (matchChara != pattern(j)) {
          isMatch = false
          matchPosition = (patternLastIndex - j)
        }
      }
      j = j - 1
    }

    (isMatch, matchChara, matchPosition)
  }

  val matchIndexes = search(text, pattern)
  println(s"出現場所: ${matchIndexes}")
}
