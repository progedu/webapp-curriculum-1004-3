object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes=BM(text,pattern)

  def BM(text:Seq[Char],pattern:Seq[Char]):Seq[Int]={
    var i=0
    var matchIndexes=Seq[Int]()
    while(i<text.length-pattern.length){
      var partText=text.slice(i,i+pattern.length)
      //println("i:"+i)
      var check=isMatch(partText,pattern)
      //println(partText)
      //println(check)
      //println("check:"+check)
      if(check==0){
        matchIndexes=matchIndexes:+i
        i=i+3;
      }else{
        i=i+check
      }
    }
    matchIndexes
  }

  def isMatch(partText:Seq[Char],pattern:Seq[Char]):Int={
    for(j <-0 to 3){
      var l=3-j
      if(partText(l)!=pattern(l)){
        //println("j :"+j)
        for(k<- 0 to 3){
          if(partText(l)==pattern(k)){
            return l-k
          }
        }
        return 4-j
      }
    }
    return 0
  }

  println(s"出現場所: ${matchIndexes}")

}