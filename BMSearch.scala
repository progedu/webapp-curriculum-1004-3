object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  //val pattern = "ドワンゴ".toSeq
  val pattern = "ワカドンゴドワ".toSeq
  // スキップテーブル作成
  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println("skipTable: " + skipTable)
  val matchIndexes = search(text, pattern)
  
  /**
   * テキストから検索ワードと一致した文字列の先頭位置を返す
   * @param {text: Seq[Char]} 検索対象のテキスト
   * @param {pattern: Seq[Char]} 検索ワード
   * @return {matchIndexes: Seq[Int]} 一致したテキストの先頭位置
   */
  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes: Seq[Int] = Seq()
    var i = 0
    
    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)
      println("partText: " + partText)
      val patternLastIndex = pattern.length - 1
      
      var isMatch = true
      var j = patternLastIndex
      var targetChar = '_'
      var mismatchPosition = 0
      
      while (j >= 0 && isMatch) {
        if (j > partText.length - 1) {
          isMatch = false
        } else {
          targetChar = partText(j)
          if (targetChar != pattern(j)) {
            isMatch = false
            mismatchPosition = (patternLastIndex - j)
          }
        }
        j = j - 1
      }
      
      if (isMatch) {
        matchIndexes = matchIndexes :+ i
        println(s"====== ${i} を登録 ======")
      }
      var skipCount = skipTable.getOrElse(targetChar, pattern.length) - mismatchPosition
      if (skipCount <= 0) skipCount = 1
      println("skipCount: " + skipCount)
      i = i + skipCount
    }
    
    matchIndexes
  }
  
  println(s"出現場所: ${matchIndexes}")
}