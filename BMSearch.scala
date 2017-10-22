import scala.util.control.Breaks.{break, breakable}

object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  //val text = "カワカドカドカドカドカドンゴドワドワンゴドカワカドカドカドドワンゴカドカドンゴドワワカワカドンゴドワドワンゴカワカドカドカドワンゴドカド".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = bmSearch(text, pattern)

  def bmSearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes: Seq[Int] = Seq()
    var table: Map[Char, Int] = Map()
    val textLen = text.length
    val patternLen = pattern.length
    var i = 0

    // create skip table
    for (i <- 0 to patternLen - 1) {
      table = table + (pattern(i) -> (patternLen - (i + 1)));
    }

    while (i < (textLen - 1)) {
      var checkChar = ' '
      var position = 0
      var j = patternLen - 1
      var skipCount = pattern.length
      val partText = text.slice(i, (i + pattern.length))

      breakable {
        while (j >= 0) {
          if (partText.length != patternLen) break       // not length match
          checkChar = partText(j)
          if (partText(j) != pattern(j)) break           // not char match
          j = j - 1
        }
      }

      if (j == -1) matchIndexes = matchIndexes :+ i   // Found
      else position = (patternLen - 1 - j)            // Not Found

      try {
        skipCount = table(checkChar) - position
      } catch {
        case e: java.util.NoSuchElementException => skipCount = pattern.length
      }
      //println(skipCount)
      i = i + skipCount
    }

    return matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}