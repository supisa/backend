val fronttag: String = "<strong>"
val backtag: String ="</strong>"
def preInsertTag(finds: Array[String], on: String): String = {
  if(!finds.isEmpty){
    val find = finds.head
    val pos: Int = on.indexOf(find)
    if (pos != -1) {
      val (frontContent, prehighlight): (String, String) = on.splitAt(pos)
      val (highlight, backContent): (String, String) = prehighlight.splitAt(find.length())


    }



    val output: String = frontContent + fronttag + highlight + backtag + backContent
    output
  }
  else on
}
