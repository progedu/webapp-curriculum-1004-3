object BMSearch extends App{
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexs = search(text, pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] ={
    var matchIndexs: Seq[Int] = Seq()

    var i = pattern.length-1
    var j = pattern.length -1
    while(i < text.length){
      if(text(i) != pattern(j)){
        j = pattern.length - 1
        val target = text(i)

        // 今回は決めうち
        var offset = pattern.length
        if( target == 'ン'){
          offset = 1
        }else if( target == 'ワ'){
          offset = 2
        }else if( target == 'ド'){
          offset = 3
        }
        i = i + offset
      }else
      {
        // 一致
        if(j == 0){
          matchIndexs = matchIndexs :+ i
          j = pattern.length - 1
          i = i + pattern.length
        }else{// 判定途中
          j = j-1
          i = i-1
        }
      }
      
    }
    matchIndexs
  }


  println(s"出現場所： ${matchIndexs}")
}