import scala.collection.mutable

object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val shiftMap = makeShiftMap(pattern)
    var matchIndexes = Seq[Int]()
    var i = 0;
    while (i + pattern.length <= text.length) {
      val partText = text.slice(i, i + pattern.length)
      println(partText)
      if (isMatch(partText, pattern)) matchIndexes = matchIndexes :+ i
      val lastChar = partText(partText.length - 1)
      i += (if (shiftMap.get(lastChar).isEmpty) pattern.length else shiftMap(lastChar))
    }
    matchIndexes
  }

  def makeShiftMap(pattern: Seq[Char]): Map[Char, Int] = {
    val patternSet = pattern.toSet;
    val shiftMap = scala.collection.mutable.Map[Char, Int]()
    for (c <- patternSet.toSeq) {
      shiftMap(c) = pattern.length - 1 - pattern.indexOf(c)
    }
    if (shiftMap(pattern(pattern.length - 1)) == 0) {
      shiftMap(pattern(pattern.length - 1)) = pattern.length
    }
    shiftMap.toMap
  }

  def isMatch(textPart: Seq[Char], pattern: Seq[Char]): Boolean = {
    var isMatch = true
    if (textPart.length < pattern.length) return false
    for (i <- pattern.length - 1 to 0 by -1) {
      if (textPart(i) != pattern(i)) return false
    }
    isMatch
  }
  println(s"出現場所: ${matchIndexes}")
}