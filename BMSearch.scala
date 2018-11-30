object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val matchIndexes = search(text, pattern)
  
  /**
    * Boyer-Moore法を用いた非索引型の文字列検索
    *
    * @param text 検索対象のテキスト
    * @param pattern 検索文字列パターン
    * @return 一致文字のインデックス
    */
  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    val skipTable = newSkipTable(pattern)
    var pt = pattern.length - 1 // textをなぞるポインタ

    while (pt < text.length) {
      var pp = pattern.length - 1 // patternをなぞるポインタ

      while (pp >= 0 && text(pt) == pattern(pp)) {
        if (pp == 0) matchIndexes = matchIndexes :+ pt
        pt -= 1
        pp -= 1
      }

      if (pp < 0) // パターンに一致した場合
        pt += pattern.length + 1
      else // 不一致文字がある場合
        pt += skipTable.getOrElse(text(pt), pattern.length)
    }
    matchIndexes
  }

  /**
    * textポインタの移動量をパターン文字ごとに計算する
    *
    * パターンに含まれない文字 -> pattern.length
    * パターンに含まれる文字 -> pattern.length - i - 1
    * 末尾文字 -> pattern.length
    * 同じ文字が含まれる場合 -> 最小のを優先
    *
    * @param pattern 検索文字列パターン移動量
    * @return 不一致文字の移動量Map
    */
  def newSkipTable(pattern: Seq[Char]): Map[Char, Int] = {
    pattern.init.zipWithIndex.map {
      case (c, i) => c -> (pattern.length - i - 1)
    }.toMap
  }

  println(s"出現場所: ${matchIndexes}")
}