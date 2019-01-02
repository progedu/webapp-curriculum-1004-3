object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println(s"skipTable: ${skipTable}")
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]() // 出現箇所のインデックスのリスト
    var i = 0 // 今回の探索の開始位置(インデックス)

    while (i < text.length - 1) { // text末尾まで実行
      val partText = text.slice(i, i + pattern.length) // インデックスからの文字列を切り出す
      println(s"partText: ${partText}")
      val patternLastIndex = pattern.length - 1 // 「パターン」の最後の文字の添字

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '_' // マッチさせた際の「文書」の文字
      var matchPosition = 0 // マッチさせた際の位置 (スキップテーブルから取得した値から差し引く値)

      while (j >= 0 && isMatch) { // 末尾から冒頭もしくはisMatchの間実行
        if (j > partText.length - 1) { // 切り出しテキストが短い際には false に
          isMatch = false
        } else {
          matchChar = partText(j) // 切り出した「文書」文字列のj番目の文字
          if (matchChar != pattern(j)) { // 切り出した「文書」と「パターン」のj番目の文字列の比較
            isMatch = false
            matchPosition = (patternLastIndex - j) // 一番後ろで不一致なら 0 、後ろから 2 番目の時に不一致なら 1 が代入
          }
        }
        j = j - 1
      }

      if (isMatch) matchIndexes = matchIndexes :+ i // 文字列が一致しているならリストに追加

      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition // 次は後ろからの不一致文字の出現位置もしくは「パターン」の長さ分ずらす
      if (skipCount <= 0) skipCount = 1 // 必ず1以上動かす
      println(s"skipCount: ${skipCount}")
      i = i + skipCount // 次のインデックスはskipCountずらした箇所
    }
    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
