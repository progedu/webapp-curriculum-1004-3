// BM法の実装
object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchedData = Seq[Int]()
    var i = 0
    while (i < text.length) {
      val partText = text.slice(i, i + pattern.length)
      val result = isMatch(partText, pattern)
      if (result._1) matchedData = matchedData :+ i
      i = i + result._2 
    }

    matchedData
  }

  def isMatch(partText: Seq[Char], pattern: Seq[Char]): (Boolean, Int) = {

    // length check
    if (partText.length < pattern.length) {
      return (false, pattern.length)
    }

    var i = 0
    var matched = true
    var appendPosition = pattern.length

    // calculate next position for search
    def nextPosition(partText: Seq[Char], pattern: Seq[Char], searchPosition: Int): Int = {
        val searchChar: Char = partText.reverse(searchPosition-1)
        val foundPosition = pattern.reverse.indexOf(searchChar, searchPosition)
        if (foundPosition > -1) foundPosition else pattern.length
    }

    // check matched text
    while (matched && i < pattern.length) {
      if (pattern(i) != partText(i))  {
        matched = false
        appendPosition = nextPosition(partText, pattern, i + 1)
      }
      i = i + 1
    }

    // calculate next position for search
    if (matched) {
      appendPosition = nextPosition(partText, pattern, 1)
    }

    // result
    (matched, appendPosition)
  }

  println(search(text, pattern))

}