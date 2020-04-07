package exercises


trait MyPredicate[-T] {
  def test(elem: T): Boolean
}


trait MyTransformer[-A, B] {
  def transform(elem: A): B
}

//MyList:
//map(transformer) => MyList
//filter(MyPredicate) => MyList
//flatMap(Transformet from A to MyList[B] => MyList[B]



abstract sealed class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  override def toString: String = "[" + printElements() + "]"

  def printElements(): String
  def filter(predicate: MyPredicate[A]): MyList[A]
  def map[B](transformer: MyTransformer[A,B]) : MyList[B]

}


object EmptyList extends MyList[Nothing] {
  def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  def printElements() = ""

  override def head: Nothing = throw new UnsupportedOperationException

  override def add[B >: Nothing](element: B): MyList[B] = new NonEmptyList[B](element, EmptyList)

  override def map[B](transformer: MyTransformer[Nothing,B]): MyList[B] =EmptyList

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = EmptyList

}

class NonEmptyList[A](val h: A, val t: MyList[A]) extends MyList[A] {
  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def printElements(): String =
    if (t.isEmpty) "" + h
    else h + ", " + t.printElements()


  override def add[B >: A](n: B): MyList[B] = new NonEmptyList(n, this)

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if(predicate.test(h)) new NonEmptyList(h,tail.filter(predicate))
    else tail.filter(predicate)
    }



//
//    if(!isEmpty){
//    if(predicate.test(h)) tail.test(predicate).add(h)
//    else tail.test(predicate)
//    }
//    else this


  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    new NonEmptyList[B](transformer.transform(h),tail.map(transformer))
  }
}

object Run extends App {
  val list = new NonEmptyList(1, EmptyList)
  val list2 = list.add(3).add(3).add(5).add(4).add(4)
  val evenPredicate = new EvenPredicate
  val evenList = list2.filter(evenPredicate)
  print(evenList.toString)

  class EvenPredicate extends MyPredicate[Int]{
    override def test(elem: Int): Boolean = elem%2 == 0
  }



}





