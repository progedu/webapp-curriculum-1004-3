object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    val shiftMap = generateBMShiftMap(pattern)

    var i = 0 //比較開始するインデックス
    var textPart = text.slice(i, i + pattern.length) //パターンと比較する部分文字列
    var diffIndex = 0 //textPartとpatternを末尾から比較した場合、初めて異なる文字が現れるIndex
    var shift = 0 // 次ループの比較で何文字ずらすか

    while (i <= text.length - pattern.length) {
      var textPart = text.slice(i, i + pattern.length)
      println("textPart:" + textPart)
      //パターン末尾からテキストを比較
      var diffIndex = getDifferentCharIndex(textPart, pattern)

      println("開始位置：" + i.toString() + "はdiffIndex:" + diffIndex.toString())

      if (diffIndex == -1) {
        //一致していればmatchIndexesに追加し、テキストの比較位置を1文字右にずらして再比較
        matchIndexes = matchIndexes :+ i
        i = i + 1
      } else {
        //一致しなければ、次のずらし幅を求めて再比較
        shift = math.max(
          1, //一つ右
          shiftMap.getOrElse(
            text(i + diffIndex),
            pattern.length
          ) - (pattern.length - 1 - diffIndex) //ずらし表から求めた次の位置
        )
        i = i + shift
      }
      println("i:" + i.toString())

    }

    matchIndexes
  }

  //textPartとpatternを末尾から比較し、異なる文字があった場合はそのIndexを返す。
  def getDifferentCharIndex(textPart: Seq[Char], pattern: Seq[Char]): Int = {
    var compareResults = Seq[Boolean]()
    for (i <- 0 until pattern.length) {
      compareResults = compareResults :+ (textPart(i) == pattern(i))
    }
    return compareResults.lastIndexWhere(n => !n)
  }

  def generateBMShiftMap(pattern: Seq[Char]): Map[Char, Int] = {
    var shiftMap = Map.empty[Char, Int]
    val distinctChars = pattern.distinct
    println("distinctChars:" + distinctChars.toString)
    for (i <- distinctChars.indices) {
      shiftMap =
        shiftMap +
          (distinctChars(i) ->
            (pattern.length - pattern.indexOf(distinctChars(i)) - 1))
    }
    println(shiftMap)
    shiftMap
  }

  println(s"出現場所: ${matchIndexes}")
}
