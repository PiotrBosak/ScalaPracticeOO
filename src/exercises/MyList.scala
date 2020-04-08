package exercises






abstract sealed class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  override def toString: String = "[" + printElements() + "]"

  def printElements(): String
  def filter(predicate: A => Boolean): MyList[A]
  def map[B](transformer: A => B) : MyList[B]
  def flatMap[B](transformer:  A => MyList[B]) : MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B]

}


case object EmptyList extends MyList[Nothing] {
  def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  def printElements() = ""

  override def head: Nothing = throw new UnsupportedOperationException

  override def add[B >: Nothing](element: B): MyList[B] = new NonEmptyList[B](element, EmptyList)

  override def map[B](transformer: Nothing =>B): MyList[B] =EmptyList

  override def filter(predicate: Nothing => Boolean ): MyList[Nothing] = EmptyList

  override def flatMap[B](transformer:  Nothing => MyList[B]): MyList[B] = EmptyList
   override def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

}

case class NonEmptyList[A]( h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h

  override def ++[B >: A](list: MyList[B]): MyList[B] = new NonEmptyList(h, tail ++ list)

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def printElements(): String =
    if (t.isEmpty) "" + h
    else h + ", " + t.printElements()


  override def add[B >: A](n: B): MyList[B] = new NonEmptyList(n, this)

  override def filter(predicate: A => Boolean )= {
    if(predicate(h)) new NonEmptyList(h,tail.filter(predicate))
    else tail.filter(predicate)
    }



  override def map[B](transformer: A => B): MyList[B] = {
    new NonEmptyList[B](transformer(h),tail.map(transformer))
  }

  override def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
  }




object Run extends App {


  val list = NonEmptyList(1, NonEmptyList(7,EmptyList))
  val list2 = NonEmptyList(5, NonEmptyList(4,EmptyList))
  val evenList = list2.filter(e => e%2 == 0)
  println(evenList.toString)
  val newList = list ++ list2
  val flatList = newList.flatMap(e => NonEmptyList(e,NonEmptyList(e+1,EmptyList)))
  println(flatList)


  }











