val fronttag: String = "<strong>"
val backtag: String ="</strong>"

def getIntersect(finds: List[String],on:String): Array[(Int,Int)] = {
  if(finds.length < 2) Array()
  else {
    val in:String = finds.head intersect finds.tail.head
    if(in.length>0)((on.indexOf(in),on.indexOf(in)+in.length)) +: getIntersect(finds.tail.tail,on)
    else Array()
  }
}
def getUnion(finds: List[String],on:String): Array[(Int,Int)] = {
  if(finds.length > 2) Array()
  else {
    val un:String = finds.head union  finds.tail.head
    if(un.length>0)((on.indexOf(un),on.indexOf(un)+un.length)) +: getIntersect(finds.tail.tail,on)
    else Array()
  }
}

def insertFront(pos: List[Int],result:String,lag: Int):String = {
  if(!pos.isEmpty && pos != -1) {
    val (f, b) = result.splitAt(pos.head - lag)
    f + fronttag + insertFront(pos.tail, b, lag + f.length-1)
  }
  else if (pos.isEmpty) result
  else ""
}

def insertBack(pos: List[Int],result:String,lag: Int ):String = {
  if(!pos.isEmpty && pos != -1){
    val (f, b) = result.splitAt(pos.head + fronttag.length - lag)
    f + backtag + insertBack(pos.tail, b, lag + f.length-1 - fronttag.length )
    }
  else if (pos.isEmpty) result
  else ""
}

def answer(finds:Array[String],on:String):String = {
  if(finds.isEmpty) on
  else if(finds.length == 1) {
    val fpos:List[Int] = List(on.indexOf(finds.head))
    val bpos:List[Int] = List(on.indexOf(finds.head)+finds.head.length)
    val result = insertBack((bpos),insertFront(fpos,on,0),0)
    result
  }
  else {
    val in: String = if (finds.isEmpty) "on" else finds.head.intersect(finds.tail).toString()
    val un: String = if (finds.isEmpty) "" else finds.head.union(finds.tail).toString()
    val dif: String = un diff(in)
    def fisrt(): (String,Boolean) = {
      if (in.length > 0) (insertBack(List(on.indexOf(in) + in.length), insertFront(List(on.indexOf(in)), on, 0), 0),true)
      else (on,false)
    }
    def second(data:String,isRep:Boolean):String = {
      if(isRep && dif.length>0) insertBack(List(on.indexOf(in) + in.length), insertFront(List(on.indexOf(in)),data,0
      ),0)
    }
  }
     }

val test = "xxxabxxx3hxxxxxxxxxxxxxxxxxxx"
val posf=List(3,7,11)
val posb=List(4,8,20)
val test1 = insertFront(posf,test,0)
val testback = insertBack(posb,test1,0)

def main(args: Array[String]) {
  //1
  val a1: Array[String] = Array("ab")
  val b1: String = "xxxxxxabxxxxx"
  val ans1 = answer(a1,b1)

  //2
  val a2: Array[String] = Array("ab","bc")
  val b2: String = "xxxxxxabxxbcxxx"
  val ans2 = answer(a2,b2)

  //3
  val a3: Array[String] = Array("abbbbb","bbb")
  val b3: String = "xxxxxxabbbbbcxxx"
  val ans3 = answer(a3,b3)
  //4
  val a4: Array[String] = Array("abbbbb","bbb")
  val b4: String = "xxxxxxabbbbbcxxx"
  val ans4 = answer(a4,b4)
//5
  val a5: Array[String] = Array("111","2222")
  val b5: String = "xxxxxxabbbbbcxxx"
  val ans5 = answer(a5,b5)

  //6
  val a6: Array[String] = Array("abbb","2222")
  val b6: String = "xxxxxxabbbbbcxxx"
  val ans6 = answer(a6,b6)

}



