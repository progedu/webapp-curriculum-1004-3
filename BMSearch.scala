import scala.math.max
object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var table = createTable(pattern)
    var i = pattern.length - 1
    var p = 0
    while ( i < text.length){
      p = pattern.length - 1
      var flag = true
      while (p >= 0 && i < text.length && flag ){
        if(text(i) == pattern(p)){
          i = i - 1
          p = p - 1
        }else{
          flag = false
        }
      }
      if (p < 0){
          matchIndexes = matchIndexes :+ (i + 1)
          i = i + pattern.length + 1
      }else{
        var shift1 = table.getOrElse(pattern(p),pattern.length)
        var shift2 = pattern.length - p
        i = i + max(shift1, shift2)
      }
    }
    matchIndexes
  }

  def createTable(pattern: Seq[Char]): Map[Char, Int] = {
    var table = Map[Char, Int]()
    for (i <- 0 until pattern.length){
      table = table + (pattern(i) -> (pattern.length - i - 1))
    }
    table
  }
  
  println(s"出現場所: ${matchIndexes}")
}