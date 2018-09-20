object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var skipTable = Map[Char, Int]()
    skipTable = setSkipTable(pattern)
    var i = 0

    while(i < text.length) {
      val partText = text.slice(i, i + pattern.length)
      var lastMatch = matchChar(partText)
      var skip = skipTable.getOrElse(lastMatch, 4)

      if (isMatch(partText, pattern)) matchIndexes = matchIndexes :+ i
      println(partText)
      i = i + skip
    }
    

    matchIndexes

  }

  def setSkipTable(pattern: Seq[Char]): Map[Char, Int] = {
    var skipTable = Map[Char, Int]()
    for(i <- 0 to pattern.length - 2) {
      val ch = pattern(i)
      val skip = 3 - i
      skipTable += (ch -> skip )
    }
    val ch = pattern(pattern.length - 1)
    skipTable += (ch -> skipTable.getOrElse(ch, 4))
    skipTable
  }
  
  def matchChar(partText: Seq[Char]): Char = {
    var i = partText.length - 1
    while(pattern(i) == partText(i)) {
      if(i == 0) return pattern(pattern.length-1)
      i -= 1
    }
    partText(i)
  }

  def isMatch(textPart: Seq[Char], pattern: Seq[Char]): Boolean =  {
    var isMatch = true
    var i = 0

    while(i < textPart.length) {
      if(textPart.length != pattern.length || textPart(i) != pattern(i)) return false
      i += 1
    }
    isMatch
  }

  println(s"出現場所: ${matchIndexes}")

}