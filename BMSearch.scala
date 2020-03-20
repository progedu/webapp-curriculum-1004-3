import util.control.Breaks._

object BMSearch extends App{
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  val answer = search(text, pattern)

  println(s"答えは　${answer}")

  def search(text:Seq[Char], pattern:Seq[Char]):Seq[Int] = {
    // This should be answer
    var matchIndexes = Seq[Int]()
    // if no option, defaultLength should be used for increment
    val defaultLength = pattern.length
    // total length of text
    val textLength = text.length
    // Using this index for slicing part text
    var searchedIndex = 0
    var endIndex = 4

    breakable{
      while(searchedIndex < textLength){
        val partText = text.slice(searchedIndex, endIndex)
        // in this context, it means comparing ends that length of part text and pattern is wrong
        if(partText.length == pattern.length){
          val result = isMatch(partText, pattern)
          if(result){
            matchIndexes = matchIndexes :+ searchedIndex
            searchedIndex = searchedIndex + pattern.length
            endIndex = searchedIndex + pattern.length
          }else{
            val nextIndex = calcNextIndex(partText, pattern, 0)
            searchedIndex += nextIndex
            endIndex = searchedIndex + pattern.length
          }
        }else{
          break()
        }
    }}
    println("finish")
    matchIndexes
  }

  def isMatch(textPart: Seq[Char], pattern: Seq[Char]): Boolean ={
    var isMatch = true
    var counter = 0
    while(isMatch&counter<pattern.length){
      if(textPart(counter) != pattern(counter)) isMatch = false
      counter +=1
    }
    isMatch
  }

  def calcNextIndex(textPart: Seq[Char], pattern: Seq[Char], counter:Int):Int = {
       val defaultLength = pattern.length
        if(textPart.reverse(counter) != pattern.reverse(counter)){
          val letter = textPart.reverse(counter)
          val indexInPattern = pattern.indexOf(letter)
          return defaultLength - (indexInPattern + 1)
        }
    calcNextIndex(textPart, pattern, counter+1)
  }

}
