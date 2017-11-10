object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  def BMSkip(str: Seq[Char]): Map[Char, Int] = {
    var skip_offset: Map[Char, Int] = Map()
    for (i <- 0 to str.length - 1 ) {
      skip_offset = skip_offset + (str(i) -> (str.length - i - 1))
    }
    skip_offset
  }

  def findBM(txt: Seq[Char], ptn: Seq[Char], start: Int): Int = {
    var bm_skip = BMSkip(ptn)
    var last = ptn.length - 1
    var pos = last + start
    println(s"pos: ${pos} bm_skip: ${bm_skip}")
    while(pos < txt.length) {
      var i = pos
      var j = last
      while ( j >= 0 && txt(i) == ptn(j)) {
        i = i - 1
        j = j - 1
      }
      var ch = txt(pos)
      var offset = bm_skip.getOrElse(txt(pos), ptn.length)
      println(s"i : ${i} j : ${j} pos : ${pos} ch : ${ch} offset : ${offset}")
      if (j < 0)  { return i + 1}
      pos += bm_skip.getOrElse(txt(i), ptn.length)
    }
    -1
  }
  
  def search(txt: Seq[Char], ptn: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var index = findBM(txt, ptn, 0)
    while (index >= 0) {
      matchIndexes = matchIndexes :+ index
      index = findBM(txt, ptn, index + 1)
    }
    matchIndexes
  }

  val result = search(text, pattern)
  println(s"出現場所：${result}")
}