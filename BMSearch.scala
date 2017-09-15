object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val matchedIndex = isMatch(pattern,text)
  println(matchedIndex);

  def createTable(pattern : Seq[Char]) : Map[Char,Int] = {
    var m = Map[Char,Int]();
    
    for (i <- 0 to (pattern.length -1)) {
      m = m ++ Map(pattern(i) -> (pattern.length - i - 1));
    }
    m
  }

  def isMatch(pattern : Seq[Char], text : Seq[Char]) : Option[Int] = {
    val n = text.length - 1
    val m = pattern.length
    
    val table = createTable(pattern)
    def getSkip(c : Char) : Int = {
      table.getOrElse(c,m)
    }

    var i = m - 1
    while (i <= n) {
      var j = m - 1;
      while (j >= 0 && text(i) == pattern(j)) {
        i -= 1
        j -= 1
      }
      if (j < 0){
        return Some(i+1)
      }
      else { 
        println("getSkip(text(i))" + getSkip(text(i)));
        i += List(getSkip(text(i)), m-j).max     
      }
    }
    None
  }
}