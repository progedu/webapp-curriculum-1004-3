object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

/*
 シフト表 shiftTable の作成　
シフト文字数はtextとpatternの文字を比べて、patternに含まれる文字の場合はpatternの末尾から見て何番目にあるかを
末尾を０として数える。patternに含まれない文字の場合はpatternの文字数とする。シフト表には文字をkeyにして、
Mapを使って key: 文字 -> value: シフト文字数として連想配列を作る。patternの末尾から数えるために挑戦初級の
lastIndexOf(string: str)を使って、pattern内で重複した文字があれば、シフト文字数少ない方を採用する。
*/
  val shiftTable = pattern.map(moji => (moji -> (pattern.length-1-pattern.lastIndexOf(moji)))).toMap
  println("シフト表: " + shiftTable)

  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes: Seq[Int] = Seq()
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)

// テキストと検索文字列の対応を検索文字列の移動で表現
      val space = "　"
      println("text:     " + text)
      println("pattern:  " + space * i + pattern)

      val patternLastIndex = pattern.length - 1

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '　' 
      var matchPosition = 0 

      while (j >= 0 && isMatch) {
        if (j > partText.length - 1 ) { 
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

      var shiftCount = shiftTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (shiftCount < 0) shiftCount = 1
      println("shiftCount: " + shiftCount)
      i = i + shiftCount
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}