object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = bmsearch(text, pattern)

  def bmsearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var skiptable = Map[Char, Int]()
    for(i <- 0 to pattern.length - 2) {
      val num = pattern.length - 1 - i
      skiptable += (pattern(i) -> num)
    }

    // println(skiptable)
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length)
      if (isMatch(partText, pattern)) matchIndexes = matchIndexes :+ i
      val skip = if(skiptable.contains(partText.last)) skiptable(partText.last) else 4
      i += skip
    }
    matchIndexes
  }

  def isMatch(textPart: Seq[Char], pattern: Seq[Char]): Boolean = {
    var isMatch = true
    var i = 0
    while (isMatch && i < pattern.length) {
      if (textPart.length < pattern.length || textPart(i) != pattern(i)) isMatch = false
      i = i + 1
    }

    isMatch
  }

  println(s"出現場所: ${matchIndexes}")
}