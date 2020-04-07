package exercises

abstract sealed class MyList[+A] {
  def head: A
  def tail : MyList[A]
  def isEmpty : Boolean
  def add[B >: A](element: B) : MyList[B]

  override def toString: String  = "[" + printElements() + "]"
  def printElements():String


}


object EmptyList extends MyList[Nothing] {
  def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  def printElements() = ""
  override def head: Nothing = throw new UnsupportedOperationException

  override def add[B >: Nothing](element: B): MyList[B] = new NonEmptyList[B](element,EmptyList)
}

class NonEmptyList[A](val h: A,val t: MyList[A]) extends MyList[A] {
  override def head: A = h

  override def tail: MyList[A] = t
  override def isEmpty: Boolean = false

  override def printElements(): String =
    if(t.isEmpty)  "" + h
    else h + ", " +t.printElements()


  override def add[B >: A](n: B): MyList[B] = new NonEmptyList(n,this)
}

object Run extends App{
  val list = new NonEmptyList(1,EmptyList)
  val list2 = list.add(3).add(3).add(5)
  println(list2.toString)
  println(list.tail  + "my tail")
  println(EmptyList.toString)
}



