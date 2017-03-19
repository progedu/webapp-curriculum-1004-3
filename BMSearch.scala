object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println("skipTable: " + skipTable)
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes: Seq[Int] = Seq()
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)
      println("partText : " + partText)
      val patternLastIndex = pattern.length - 1 // 「パターン」の最後の文字の添字

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '_' // マッチさせた際の「文書」の文字
      var matchPosition = 0 // マッチさせた際の位置（スキップテーブルから取得した値から差し引く値）

      while(j >= 0 && isMatch) {
        if (j > partText.length - 1) { // 切り出しテキストより短い場合はfalse
          isMatch = false
        } else {
          matchChar = partText(j)
          if (matchChar != pattern(j)) {
            isMatch = false
            matchPosition = (patternLastIndex - j) // 一番後ろで不一致なら0が代入、後ろから2番目が不一致なら1が代入

          }
        }
        j = j -1
      }

      if(isMatch) matchIndexes = matchIndexes :+ i

      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (skipCount < 0) skipCount = 1
      println ("skipCount: " + skipCount)
      i = i + skipCount
    }

    matchIndexes
  }
  
  println(s"出現場所: ${matchIndexes}")
}