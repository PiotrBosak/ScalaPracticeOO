package exercises

abstract sealed class MyList {
  def head: Int
  def tail : MyList
  def isEmpty : Boolean
  def add(n :Int) : MyList

  override def toString: String  = "[" + printElements() + "]"
  def printElements():String


}


object EmptyList extends MyList {
  def head: Int = throw new NoSuchElementException
  def tail: MyList = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  def printElements() = ""

  override def add(n: Int): MyList = new NonEmptyList(n,EmptyList)
}

class NonEmptyList(val h: Int,val t: MyList) extends MyList {
  override def head: Int = h

  override def tail: MyList = t
  override def isEmpty: Boolean = false

  override def printElements(): String =
    if(t.isEmpty)  "" + h
    else h + ", " +t.printElements()


  override def add(n: Int): MyList = new NonEmptyList(n,this)
}

object Run extends App{
  val list = new NonEmptyList(1,EmptyList)
  val list2 = list.add(3).add(3).add(5)
  println(list2.toString)
  println(list.tail  + "my tail")
  println(EmptyList.toString)
}



