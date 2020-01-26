object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0
    while (i <= text.length - pattern.length) {
      val partText = text.slice(i, i + pattern.length)
      var moveToNextPart = false
      var j = pattern.length - 1
      while (!moveToNextPart) {
        if (j == -1) {
          matchIndexes = matchIndexes :+ i
          i = i + skipLength(pattern.slice(0, pattern.length - 1), partText(pattern.length - 1)) + 1
          moveToNextPart = true
        } else if (partText(j) != pattern(j)) {
          i = i + skipLength(pattern.slice(0, j + 1), partText(j))
          moveToNextPart = true
        } else {
          j = j - 1
        }
      }
    }
    matchIndexes
  }

  def skipLength(pattern: Seq[Char], char: Char): Int = {
    val index = pattern.reverse.indexOf(char)
    if (index < 0) pattern.length else index
  }

  println(s"出現場所: ${matchIndexes}")
}