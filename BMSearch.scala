object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0
    var j = 0
    var k = 0
    var isMatch = true

    while (i <= text.length - pattern.length) {
      isMatch = true
      val partText = text.slice(i, i + pattern.length)

      // 切り出した文字列が"ドワンゴ"かチェック
      j = pattern.length - 1
      while (j >= 0 && isMatch){
        if (partText.length < pattern.length || partText(j) != pattern(j)) isMatch = false
        if (isMatch) j -= 1
      }

      //"ドワンゴ"のときは文字位置を記録
      if (isMatch) matchIndexes = matchIndexes :+ i; j = 0;

      //"次のスタート位置を決定"
      k = 0
      while (k < pattern.length && j != 0) {
        if (partText(j) == pattern(k)) {
          i += pattern.length - k - 1 
          k = pattern.length + 1
        } else {
          k += 1
        }
      }
      if (k == pattern.length || j == 0) i += pattern.length

    }
    matchIndexes
  }
  println(s"出現場所: ${matchIndexes}")
}
