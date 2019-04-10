object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  println(bmSearch(text, pattern))
  
  def bmSearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    def loop(index: Int, text: Seq[Char], pattern: Seq[Char], skipTable: Map[Char, Int], acc: Seq[Int]): Seq[Int] = {
      if (text.length < pattern.length || index + pattern.length > text.length) acc
      else {
        val txt = text.slice(index, index + pattern.length)
        isMatch(txt, pattern, skipTable) match {
          case Right(n) => loop(index + n, text, pattern, skipTable, acc :+ index)
          case Left(n) => loop(index + n, text, pattern, skipTable, acc)
        }
      }
    }
    val skipTable = createSkipTable(pattern)
    loop(0, text, pattern, skipTable, Seq(): Seq[Int])
  }

  // スキップテーブルを返す
  def createSkipTable(pattern: Seq[Char]): Map[Char, Int] = {
    pattern.zipWithIndex.foldLeft(Map(): Map[Char, Int])((acc, x) => acc + (x._1 -> (pattern.length - x._2 - 1)))
  }

  //  パターンとテキストが一致したかとインデックスの移動数を返す
  def isMatch(text: Seq[Char], pattern: Seq[Char], skipTable: Map[Char, Int]): Either[Int, Int] = {
    def loop(index: Int, text: Seq[Char], pattern: Seq[Char], skipTable: Map[Char, Int]): Either[Int, Int] = text(index) == pattern(index)match {
      case true => {
        if (index == 0) Right(1) else loop(index - 1, text, pattern, skipTable)
      }

      case false => Left(skipTable.getOrElse(text(index), pattern.length) - (pattern.length - index - 1))
    }
    loop(pattern.length - 1, text, pattern, skipTable)
  }
}