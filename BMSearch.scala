object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  // (引数) => 処理
  // Mapでは key -> value　で管理する
  val shiftTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println("ずらし表（照合再開位置を示す表）(shiftTable): " + shiftTable) // Map(ド -> 3, ワ -> 2, ン -> 1, ゴ -> 0)
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)
      println("partText: " + partText)
      val patternLastIndex = pattern.length - 1 // 「パターン」の最後の文字の添字

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '_' // マッチさせた際の「文書」の文字
      var matchPosition = 0 // マッチさせた際の位置 (ずらし表(shiftTable)から取得した値から差し引く値)

      // && は　A かつ B　ということ
      while (j >= 0 && isMatch) {
        if (j > partText.length - 1 ) { // 切り出しテキストが短い際には false に
          isMatch = false
        } else {
          matchChar = partText(j)
          if (matchChar != pattern(j)) {
            isMatch = false
            matchPosition = (patternLastIndex - j)  // 一番後ろで不一致なら 0 、後ろから 2 番目の時に不一致なら 1 が代入
          }
        }
        j = j - 1
      }

      if (isMatch) matchIndexes = matchIndexes :+ i

      var shiftCount = shiftTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (shiftCount <= 0) shiftCount = 1
      println("ずらし幅(shiftCount): " + shiftCount)
      i = i + shiftCount
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}