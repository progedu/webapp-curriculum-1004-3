object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  def BMSearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var shiftTable = creatShiftTable(pattern)
    var textIndex = 0
    textIndex = pattern.length - 1
    while(textIndex < text.length) {
      var patternIndex = pattern.length - 1
      var originIndex = textIndex
      var isMatch = true
      while (isMatch && patternIndex >= 0) {
        if(text(textIndex) != pattern(patternIndex)) {
          isMatch = false
          textIndex = getNextTargetIndex(originIndex, textIndex)
        } else {
          textIndex = textIndex - 1
          patternIndex = patternIndex -1
        }
      }
      if(patternIndex < 0) {
        matchIndexes = matchIndexes :+ textIndex + 1
        textIndex = originIndex + 1
      }
    }
    def getNextTargetIndex(originIndex: Int, textIndex: Int): Int = {
      var nextIndex:Int = 0
      var shiftNum = shiftTable.getOrElse(text(textIndex), pattern.length)
      if (originIndex < textIndex + shiftNum) {
        nextIndex = textIndex + shiftNum
      } else {
        nextIndex = originIndex + 1
      }
      nextIndex
    }
    matchIndexes
  }

  def creatShiftTable(pattern: Seq[Char]): Map[Char, Int] = {
    var shiftTable = Map[Char, Int]()
    for(i <- 0 until pattern.length) {
      shiftTable = shiftTable + (pattern(i) -> (pattern.length - i - 1))
    }
    shiftTable
  }

println(s"出現位置：${BMSearch(text, pattern)}")
}