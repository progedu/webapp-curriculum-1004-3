object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text,pattern)
  //skipTable
  def skipNum (anyChar: Char): Int ={

    anyChar match {
      case 'ド' => 3
      case 'ワ' => 2
      case 'ン' => 1
      case 'ゴ' => 0
      case _ => 4
    }
  }
  
  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val patLen = pattern.length
    var matchIndexes = Seq[Int]()
    var i = 0
    while (i <= text.length - patLen) {

      var partText = text.slice(i,i + patLen)
      var ii = i // テキストを切り出して、以下のループに入る。 iの値が変わったら、ループを抜ける。という処理のためにiiという変数を使う
      var count = patLen - 1
       
        while(ii == i) {//移動距離が決まるまでループする。
          if (partText(count) == pattern(count)){//照合する。 
            if (count==0){matchIndexes = matchIndexes :+ i; i= i+4//
            } else {count = count -1} //照合する箇所を一つずらす。
          } else {
          i = i + skipNum(partText(count))//間違っていた時点で、スキップする。
        }
      }
    }
    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}
//テキストを少し変えるとうまく動かないためなんかおかしい。