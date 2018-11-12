import scala.math.max
object BMSearch extends App {
 
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq

  // BMサーチ実行
  val results = bmSearch(text, pattern)
  println(results)

  def bmSearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    val skipTable = CreateTable(pattern)  // スキップテーブル：移動幅を事前に計算して連想配列に保存
    var resultSeq = Seq[Int]()            // 結果シーケンス：テキスト内で、パターンが発見された場所を保存

    var textIndex = pattern.length - 1  // 探索開始位置をパターンの末尾インデックスで初期化
    var patternIndex = 0

    // 探索開始
    while (textIndex < text.length) {
      patternIndex = pattern.length - 1
      var judge = true // パターンと一致しない文字が出現したらfalseにする
      while (patternIndex >= 0 && judge) {
        if (text(textIndex) == pattern(patternIndex)) {
          // 文字が一致したら探索インデックスを左に移動して次の文字同士を比較
          textIndex = textIndex - 1
          patternIndex = patternIndex - 1
        }
        else {
          // 一致しない文字が出現したら判定をfalseにして内側のループ終了、移動幅を計算して次の探索へ
          judge = false
        }
      }

      if (patternIndex < 0) {
        // 全ての単語が一致
        println("BINGO!")
        resultSeq = resultSeq :+ (textIndex + 1)  // 結果シーケンスに保存
        textIndex = textIndex + 1 + pattern.length  // 探索開始インデックスを更新（パターンの長さ分丸々右にシフト）
      } else {
        // 一致しない文字が存在した場合、移動幅計算
        var skipDistance = CalcSkipDistance(text, pattern, skipTable, patternIndex, textIndex)
        textIndex = textIndex + skipDistance   // 探索開始インデックスを更新 
      }
    } 
    resultSeq
  }  

  // 移動幅の計算
  def CalcSkipDistance(text: Seq[Char], pattern: Seq[Char], table: Map[Char, Int], currentPatternIndex: Int, currentTextIndex: Int): Int = {    
    // 一致しなかった文字がパターンに存在するなら、連想配列から移動幅を取得、存在しなければパターンの文字の長さを取得
    var skip1 = table.getOrElse(text(currentTextIndex), pattern.length)

    // 今回の探索でいくつインデックスが左に進んだか（何文字一致したか）を計算。移動幅の計算に必要
    var counter = pattern.length - currentPatternIndex -1  
    
    // 探索インデックスが左に進んだ分を加味して、移動幅を調整（一致した文字の数だけ移動幅を減らす）
    // 不一致文字 “x” がパターンに含まれない ：　その文字の１文字右の位置から次回の探索が始まるように
    // 不一致文字 “x” がパターンに含まれる ：　パターン内の最も右の"x"がテキスト内の"x"の位置から次回の探索が始まるように
    skip1 = skip1 - counter

    // 移動幅の計算結果がマイナスになる場合
    // 左への移動は禁止なので、今回探索した開始インデックスから１文字右にシフトするような移動幅にする
    var skip2 = pattern.length - currentPatternIndex

    max(skip1, skip2)
  }

  // パターン内の文字に対応する移動幅の連想配列テーブルを作成
  def CreateTable(pattern: Seq[Char]): Map[Char, Int] = {
    var skipTable = Map[Char, Int]()
    for (i <- 0 until pattern.length) {
      skipTable = skipTable + (pattern(i) -> (pattern.length - i - 1))
    }
    skipTable
  }
}

