package org.gs.search

import scala.annotation.tailrec
import scala.math.max

object BMSearch extends App {
  val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
  val pattern = "ドワンゴ".toSeq
  val skipTable = pattern.map(s => (s -> (pattern.reverse.indexOf(s)))).toMap
  println("skipTable: " + skipTable) // Map(ド -> 3, ワ -> 2, ン -> 1, ゴ -> 0)
  val matchIndexes = bmsearch(text, pattern)

  def bmsearch(text: Seq[Char], pattern: Seq[Char]): Seq[Int] = {
    var matchIndexes = Seq[Int]()
    var i = 0

    while (i < text.length - 1) {
      val partText = text.slice(i, i + pattern.length) //iを開始位置としてtextから切り抜いた4文字の文字列
      println("partText: " + partText)
      val patternLastIndex = pattern.length - 1 // 「パターン」の最後の文字の添字、つまり3

      var isMatch = true
      var j = patternLastIndex
      var matchChar = '_' // マッチさせた際の「文書」の文字
      var matchPosition = 0 // マッチさせた際の位置 (スキップテーブルから取得した値から差し引く値)

      while (j >= 0 && isMatch) {
        if (j > partText.length - 1 ) { // 切り出しテキストが短い際には false に
          isMatch = false
        } else {
          matchChar = partText(j) //一致した部分の1文字(char)を代入
          if (matchChar != pattern(j)) {
            isMatch = false
            matchPosition = (patternLastIndex - j)  // 一番後ろで不一致なら 0 、後ろから 2 番目の時に不一致なら 1 が代入
          }
        }
        j = j - 1
      }

      if (isMatch) matchIndexes = matchIndexes :+ i //マッチした際に最初の文字の位置（i）をリストに追加

      var skipCount = skipTable.getOrElse(matchChar, pattern.length) - matchPosition
      if (skipCount <= 0) skipCount = 1
      println("i=" + i + ", skipCount: " + skipCount) //フィボナッチ数列っぽい？
      i = i + skipCount
    }

    return matchIndexes
  }

  println(s"出現場所: ${matchIndexes}")
}