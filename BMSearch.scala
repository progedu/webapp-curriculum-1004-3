object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  // val text = "アアアアアアア".toSeq
  // val pattern = "アアアア".toSeq
  // val text = "アイアイアイアイ".toSeq
  // val pattern = "アイアイ".toSeq
  // val text = "アイアイアイアイア".toSeq
  // val pattern = "ニク".toSeq


  // 文字をずらす数の連想配列
  var skipTable = Map[Char, Int]()

  for (i <- 0 to pattern.length - 1) {
    skipTable = skipTable + (pattern(i) -> (pattern.length - i - 1))
  }
  println(skipTable)

  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()

    // 文書から、比較する文字列を取り出す最後の位置
    var lastPos = pattern.length

    while (lastPos <= text.length) {
      val firstPos = lastPos - pattern.length
      val partText = text.slice(firstPos, lastPos)
      println("partText: " + partText)

      // 後ろから比較する
      var isMatch = true
      var i = pattern.length - 1;
      while (isMatch && i > 0) {
        if (partText(i) != pattern(i)) {
          // 一致しなかった文字が、skipTableにあればそれでずらし、なければパターンの文字数分ずらす
          lastPos = lastPos + skipTable.getOrElse(partText(i), pattern.length)
          isMatch = false
        }
        i = i - 1;
      }

      if (isMatch) { 
        matchIndexes = matchIndexes :+ firstPos
        var temp = skipTable(pattern(0))
        if (temp == 0) temp = 1
        lastPos = lastPos + temp
      }
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
