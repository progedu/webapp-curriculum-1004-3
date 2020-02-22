object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  //val pattern = "カワカド".toSeq
  println(search(text, pattern))
  
  def patternShiftMake(pattern: Seq[Char]): Map[Char, Int] = {
    var patternShift = Map[Char, Int]()
    //for (i <- pattern.length - 1 to 0 by -1) {
    for (i <- 0 until pattern.length - 1) {
      patternShift = patternShift + (pattern(i) -> (pattern.length - i - 1))
    }
    patternShift
  }

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val patternShift = patternShiftMake(pattern)
    val ptnLenM = pattern.length - 1
    var matchIndexes = Seq[Int]()
    var i = 0
    var j = ptnLenM
    
    println(patternShift)
    while (i < text.length - pattern.length && j > -1) {
      var partText = text.slice(i, i + pattern.length)
      println(s"i:${i}, j:${j}, partText:${partText}, ${partText(j)}？${pattern(j)}")
      if(partText(j) != pattern(j)) {
        println(s"Unmatch now i:${i}, + Shift:${patternShift.getOrElse(partText(j), pattern.length)}")
        i = i + patternShift.getOrElse(partText(j), pattern.length)
        j = ptnLenM
        println(s"Unmatch then i:${i}, j:${j}")
      } else {
        if (j > 0) {
          j = j - 1
        } else {
          matchIndexes = matchIndexes :+ i
          i = i + pattern.length
          j = ptnLenM
          println(s"Match then i:${i}, j:${j}, matched:${matchIndexes}")
        }
      }
    }
    matchIndexes
  }

}