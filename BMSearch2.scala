object BMSearch2 extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワンゴンゴンゴンゴンドドドドドドワンゴ".toSeq
  val pattern = "ドワンゴ".toSeq
  val matchIndexes = search(text,pattern)

  def search(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
    println(skipTable)
    val patLen = pattern.length
    var matchIndexes = Seq[Int]()
    var i = 0
    while (i <= text.length - patLen) {
      var partText = text.slice(i,i + patLen)
      println()
      println(partText)
      var ii = i // テキストを切り出して、以下のループに入る。 iの値が変わったら、ループを抜ける。という処理のためにiiという変数を使う
      var checkedPointOfPartText = patLen - 1 // 切り出したテキストのどこを照合しているか
       
        while(ii == i) {//移動距離が決まるまでループする。
          if (partText(checkedPointOfPartText) == pattern(checkedPointOfPartText)){//照合する。 
            if (checkedPointOfPartText==0){matchIndexes = matchIndexes :+ i; i= i+4; println("Success! : i =" + i)//最後まで照合できた場合。
            } else {checkedPointOfPartText = checkedPointOfPartText -1} //照合する箇所を一つずらす。
          } else {//マッチしなかったら。
            if (!skipTable.contains(partText(checkedPointOfPartText))){//skipTableにない文字を照合している場合。
              i = i + patLen - (3 - checkedPointOfPartText);println(partText(checkedPointOfPartText) + "はpattern内に含まれていないためスキップしました。: i =" + i)
            }else{ 
              if (3 - skipTable(partText(checkedPointOfPartText)) > checkedPointOfPartText ) {//partTextに,ンゴンゴやンドドドのような同じ文字の繰り返しが出てきた場合の処理。。
                i = i + checkedPointOfPartText + 1//照合している文字の隣に、検証開始地点を移動する。
              } else {
                i = i + skipTable(partText(checkedPointOfPartText)) - (3 - checkedPointOfPartText)//移動距離　＝ skipTableででた数値 - 照合が合わない場所の、後ろからの距離 
              }
              
              println(partText(checkedPointOfPartText) + "(" + checkedPointOfPartText + ")" + "と" + pattern(checkedPointOfPartText) + "がマッチしていませんが、”"+ partText(checkedPointOfPartText)+"”がパターンに含まれているので、それに応じて移動しました。: i =" + i)//ここおかしいかも。
          }
        }
      }
    }
    matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}