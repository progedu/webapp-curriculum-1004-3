object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def patternShiftMake(pattern: Seq[Char]): Map[Char, Int] = {
    var patternShift = Map[Char, Int]()
    for (i <- 0 until pattern.length - 1) {
      patternShift = patternShift + (pattern(i) -> (pattern.length - i - 1))
    }
    patternShift
  }

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val patternShift = patternShiftMake(pattern)
    val ptnLenM = pattern.length - 1
    var matchIndexes = Seq[Int]()
    var i = 0
    var j = ptnLenM
    
    while (i < text.length - pattern.length && j > -1) {
      var partText = text.slice(i, i + pattern.length)
      if(partText(j) != pattern(j)) {
        i = i + patternShift.getOrElse(partText(j), pattern.length)
        j = ptnLenM
      } else {
        if (j > 0) {
          j = j - 1
        } else {
          matchIndexes = matchIndexes :+ i
          i = i + pattern.length
          j = ptnLenM
        }
      }
    }
    matchIndexes
  }

  println(s"出現場所： ${matchIndexes}")
}