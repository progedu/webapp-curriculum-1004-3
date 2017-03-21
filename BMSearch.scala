object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val skipTable = pattern.zipWithIndex.map {
    case (c: Char, i: Int) => c -> (pattern.length - 1 - i)
  }.toMap
  println(skipTable)
  val matchIndexes = search(text, pattern)

  //以下同じ文字が複数ある場合の動作確認
  val testPattern = "ドワドゴド".toSeq
  val testTable = testPattern.zipWithIndex.map {
    case (c: Char, i: Int) => c -> (testPattern.length - 1 - i)
  }.toMap
  println(s"テスト用: ${testTable}")
  //ここまで

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes: Seq[Int] = Seq()
    var i = 0

    while (i < text.length - pattern.length + 1) {
      val partText = text.slice(i, i + pattern.length)
      //文字数一致なら
      if (partText.length == pattern.length) {
        val shiftNum = isMatch(partText, pattern)
        //shiftNum=0 は一致
        if (shiftNum == 0) {
          println("match!!!!!!!!!!!!!!!!!!!")
          matchIndexes = matchIndexes :+ i
          i += 1
        }
        //一致しない => 必要な分ずらす
        else i += shiftNum
      }
    }

    matchIndexes
  }

  def isMatch(textPart: Seq[Char], pattern: Seq[Char]): Int = {
    //shiftNum=0 は一致
    var shiftNum = 0
    var i = pattern.length - 1

    while (i >= 0) {
      println(s"Compare ${textPart(i)} and ${pattern(i)}")
      //1文字比較
      if (textPart(i) == pattern(i)) i -= 1
      //一致しない
      else {
        var involved = false
        var j = 0

        //patternに含まれているか
        while (j <= pattern.length - 1 && !involved) {
          println(s"Involved in pattern? Compare ${textPart(i)} and ${pattern(j)}")
          if (textPart(i) == pattern(j)) involved = true
          else j += 1
        }

        //含まれる
        if (involved) shiftNum = skipTable(textPart(i)) - (pattern.length - 1 - i)
        //含まれない
        else shiftNum = pattern.length
        println(s"shift = ${shiftNum}")
        i = -1
      }
    }

    shiftNum
  }


  println(s"出現場所: ${matchIndexes}")
}