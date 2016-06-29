def answer(finds:Array[String],on:String):String = {
  val combination = finds.toSet[String].subsets.map(_.toList).toList.filter(x=>x!=List())
  def getIntersect(list: List[String]): List[String] = list.tail.map(_.intersect(list.head)).distinct
  val intersction = (for(i <- 1 to combination.length-1) yield getIntersect(combination(i))).toList.filter(x=>x!=List())
  def getOrder(finds:List[List[String]],order:Int):List[(Int,Int,Int)] ={ // (oder,frontpostion,backposition)
    if(finds.isEmpty) List()
    else {
      val find:String = finds.last.maxBy(_.length)
      (order,on.indexOf(find),on.indexOf(find) + find.length-1) :: getOrder(finds.init,order+1)
    }  //getOrder(combination,1)
  }
}
val combination = List(1,2,3).toSet[Int].subsets.map(_.toList).toList