object BMSearch extends App {
    val text = "カワカドカドカドドワンゴカドカドンゴドワドワンゴドワカワカドンゴドワ".toSeq
    val pattern = "ドワンゴ".toSeq

    def createIndex(pattern: Seq[Char]) : Map[Char,Int] = {
        var res = Map[Char,Int]()
        for (i <- 0 until pattern.length) {
            val x = pattern.length-i-1
            res += (pattern(i) -> x)
        }
        res
    }

    val bmIndex = createIndex(pattern)
    println(bmIndex)

    var matchIndexes = Seq[Int]()

    var i = 0
    while(i+pattern.length-1 < text.length) {
        var ok = true
        var j = pattern.length -1
        while(ok && j >= 0) {
            println(i,j)
            if (text(i+j) != pattern(j)) ok = false
            else j -= 1
        }
        if (ok) {
            matchIndexes = matchIndexes :+ i
            i += pattern.length
        } else i += bmIndex.getOrElse(text(i+j),pattern.length)
    }

    println(matchIndexes)

}