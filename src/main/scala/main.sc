val fronttag: String = "<strong>"
val backtag: String ="</strong>"
def preInsertTag(find: String, on: String): String = {
  val pos: Int = on.indexOf(find)
  if (pos != -1) {
    val (frontContent, prehigtlight): (String, String) = on.splitAt(pos)
    val (higtlight, backContent): (String, String) = prehigtlight.splitAt(find.length())
    val output: String = frontContent + fronttag + higtlight + backtag + backContent
    output
  }
  else on
}

def insertTag(finds: Array[String], on: String): String = {
 val preOutput: Array[String] = finds.map { x => preInsertTag(x, on) }
  if(preOutput.length != 1 ) preOutput(0).intersect(preOutput(1))
  else preOutput.toString()
}

val a1 = Array("ab")
val b1 = "xxxxxabxxxx"
insertTag(a1,b1)

val a2 = Array("ab","bc")
val b2 = "xxxxxabxxbcxx"
insertTag(a2,b2)
