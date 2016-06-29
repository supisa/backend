val fronttag: String = "<strong>"
val backtag: String ="</strong>"
def preInsertTag(finds: Array[String], on: String): String = {
  val intersection = finds(0) intersect(finds(1))
  if(intersection.length > 0) {
    val pos: Int = on.indexOf(intersection)
    val (frontContent, prehigtlight): (String, String) = on.splitAt(pos)
    val (higtlight, backContent): (String, String) = prehigtlight.splitAt(intersection.length())
    val output: String = frontContent + fronttag + higtlight + backtag + backContent
    preInsertTag(finds,on)
  }

}