import scala.math

object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  var matchIndexes = bmSearch(text, pattern)

  def bmSearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val textLen = text.length
    val patLen = pattern.length
    var matchIndexes = Seq[Int]()
    var skipTable: Map[Char, Int] = Map()
    for (i <- 0 to patLen - 1) {
      skipTable = skipTable + (pattern(i) -> (patLen - i - 1))
    }
    //println(skipTable)
    var i = patLen - 1
    while (i < textLen) {
      var j = patLen - 1
      var loopEnd = false
      while (!loopEnd && text(i) == pattern(j)) {
        if (j == 0) {
          matchIndexes = matchIndexes :+ i
          loopEnd = true
        } else {
          i = i - 1
          j = j - 1
        }
      }
      i = i + math.max(patLen - j, skipTable.getOrElse(text(i), patLen))
    }

    matchIndexes
  }

  println(matchIndexes)
}