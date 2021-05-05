object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexed = search(text, pattern)

  def createBmTable(pattern: Seq[Char]): Map[Char, Int] = {
    var bmTable = Map[Char, Int]()
    for(i <- 0 to pattern.length - 1) {
      bmTable = bmTable + (pattern(i) -> (pattern.length - i - 1))
    }
    bmTable
  }

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
      val bmTable = createBmTable(pattern);
      var matchIndexes = Seq[Int]()

      var i = pattern.length - 1;// テキストの比較位置
      var j = pattern.length - 1;// パターンの比較位置

      while(i < text.length) {
        if (text(i) != pattern(j)) {
          if(bmTable.contains(text(i))) i += bmTable(text(i)) else i += pattern.length
          j = pattern.length - 1
        } else {
          if(j == 0) {
            matchIndexes = matchIndexes :+ i
            i += pattern.length
            j = pattern.length - 1;
          }else {
            i -= 1
            j -= 1
          }
        }
      }
      matchIndexes
  }
  println(s"出現位置: ${matchIndexed}")
}