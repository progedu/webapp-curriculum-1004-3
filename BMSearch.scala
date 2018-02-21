object BMSearch extends App {
  val TEXT = "ドワンゴカワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワンゴ".toSeq
  val PATTERN = "ドワンゴ".toSeq

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val TXT_LEN = text.length
    val PTN_LEN = pattern.length
    // SKIP : Map('ド' -> 3, 'ワ' -> 2, 'ン' -> 1, 'ゴ' -> 0)
    val SKIP = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
    var matchIdxs = Seq[Int]()
    var i = 0

    while (i <= TXT_LEN - PTN_LEN) {
      val PART = text.slice(i, i + PTN_LEN)
      println("PART: " + PART)

      var isMatch = true;
      var j = PTN_LEN - 1;
      var matchCh = '_'
      var matchPos = 0

      while (j >= 0 && isMatch) {
        matchCh = PART(j)
        if (matchCh != pattern(j)) {
          isMatch = false
          matchPos = PTN_LEN - 1 -j
        }
        j -= 1
      }

      if (isMatch) matchIdxs = matchIdxs :+ i

      var skipCnt = SKIP.getOrElse(matchCh, PTN_LEN) - matchPos
      if (skipCnt <= 0) skipCnt = 1
      println("skipCnt: " + skipCnt)
      i += skipCnt
    }

    matchIdxs;
  }

  val matchIndexes = search(TEXT, PATTERN)
  println(s"出現場所: ${matchIndexes}")
}