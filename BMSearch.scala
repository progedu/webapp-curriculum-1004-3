object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val rev = pattern.reverse
  val table = pattern.map(c => (c -> rev.indexOf(c))).toMap

  def bm(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var indexes = Seq[Int]()
    var i = 0

    while (i < text.length - 1) {
      val textPart = text.slice(i, i + pattern.length)
      var j = pattern.length - 1
      var pos = 0
      var letter = '_'
      var isMatch = true

      while (j >= 0 && isMatch) {
        if (j >= textPart.length) {
          isMatch = false
        } else {
          letter = textPart(j)
          if (letter != pattern(j)) {
            isMatch = false
            pos = pattern.length - 1 - j
          }
          j -= 1
        }
      }

      if (isMatch) {
        indexes :+= i
      }
      
      var skipCount = table.getOrElse(letter, pattern.length) - pos
      if (skipCount < 1) {
        skipCount = 1
      }
      i += skipCount
    }

    indexes
  }

  println(s"出現場所: ${bm(text, pattern)}")
}
