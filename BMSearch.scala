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
      println("partText: " + partText)
      val patternLastIndex = pattern.length - 1 // この場合は 3

      var isMatch = true
      var j = patternLastIndex // 末尾から調べる
      var matchChar = '_' // pattern に含まれない文字ならなんでもいい
      var matchPosition = 0

      while (j >= 0 && isMatch) { 
        if (j > partText.length - 1) { // partText のほうが短い
          isMatch = false
        } else {
          matchChar = partText(j)
          if (matchChar != pattern(j)) {
            isMatch = false
            matchPosition = (patternLastIndex - j) // 末尾から見て一致している文字数
            println("matchPosition: " + matchPosition)
          }
        }
        j = j - 1
      }
      // isMatch == true のまま while を抜けたならば pattern が見つかったことになる
      if (isMatch) matchIndexes = matchIndexes :+ i

      // matchChar が skipTable になければ skipCount = pattern.length - matchPosition
      // さらに末尾が不一致ならば matchPosition = 0 なので pattern.length 文字スキップできる
      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (skipCount < 0) skipCount = 1 // 後戻りしないようにする
      println("skipCount: " + skipCount)
      i = i + skipCount
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
