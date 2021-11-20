object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i: Int = 0
    var j: Int = 0
    var textIndex: Int = 0
    var chk: Int = 0
    var skip: Int = 0
    i = pattern.length - 1

    while(text.length > i) {
      chk = 0
      skip = 0
      j = pattern.length - 1
      textIndex = i
      while (j >= 0) {
        if (text(textIndex) != pattern(j)) {
          chk = chk + 1
        }
        if (text(textIndex) == pattern(0) && j > 0) {
          skip = j
        }
        textIndex = textIndex - 1
        j = j - 1
      }

      if (chk == 0) {
        matchIndexes = matchIndexes :+ (i - (pattern.length - 1))
        i = i + pattern.length
      } else if (skip != 0) {
        i = i + skip
      } else {
        i = i + chk
      }       
    }
    
    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}