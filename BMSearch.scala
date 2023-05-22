object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq //文字列をせーけんすに変換
  val pattern = "ドワンゴ".toSeq //文字列をシーケンスに変換
  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap //パターンの各文字が末尾から何もじめにあるかを示す表を作成
  // println("skipTable: " + skipTable) // Map(ド -> 3, ワ -> 2, ン -> 1, ゴ -> 0)
  val matchIndexes = search(text, pattern)//　パターンがテキスト中で一致する位置（インデックス）のシーケンスを取得

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()//マッチしたインデックスを保持するためのシーケンス
    var i = 0//テキストの捜査を監視する位置を０から開始

    while (i < text.length - 1) {//テキストの終わりまでループ、iがテキストの長さより小さい間、ループの中のコードが実行される
      val partText = text.slice(i, i + pattern.length)//テキストからパターンと同じ長さの部分文字列を切り出す
      val patternLastIndex = pattern.length - 1 // 「パターン」の最後の文字の添字

      var isMatch = true//部分文字列がパターンに一致しているかどうかを示す
      var j = patternLastIndex//部分文字列をパターンを逆順に比較するためにつかう
      var matchChar = '_' // マッチさせた際の「文書」の文字
      var matchPosition = 0 // マッチさせた際の位置 (スキップテーブルから取得した値から差し引く値)

      while (j >= 0 && isMatch) {//部分文字列とパターンを後ろから前に向かって一文字ずつ比較。どちらかの文字が一致しなｋった。または、部分文字列が短すぎて比較できなかったときに、falseにする
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

      if (isMatch) matchIndexes = matchIndexes :+ i//一致していたら部分文字列の開始位置を追加

      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition //次に進む文字数を計算。スキップテーブルから不一致だった文字の値を取得。存在しない場合はpターンの長さを仕様。マッチした位置を差し引くことで求める
      if (skipCount <= 0) skipCount = 1//少なくとも１文字はスキップする必要がある。
      i = i + skipCount
    }

    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
