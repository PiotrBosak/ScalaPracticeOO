package exercises

import scala.collection.immutable.Stream.Cons


trait MyPredicate[-T] {
  def test(elem: T): Boolean
}


trait MyTransformer[-A, B] {
  def transform(elem: A): B
}



abstract sealed class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  override def toString: String = "[" + printElements() + "]"

  def printElements(): String
  def filter(predicate: MyPredicate[A]): MyList[A]
  def map[B](transformer: MyTransformer[A,B]) : MyList[B]
  def flatMap[B](transformer: MyTransformer[A,MyList[B]]) : MyList[B]
  def ++[B >: A](list: MyList[B]): MyList[B]

}


case object EmptyList extends MyList[Nothing] {
  def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  def printElements() = ""

  override def head: Nothing = throw new UnsupportedOperationException

  override def add[B >: Nothing](element: B): MyList[B] = new NonEmptyList[B](element, EmptyList)

  override def map[B](transformer: MyTransformer[Nothing,B]): MyList[B] =EmptyList

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = EmptyList

  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = EmptyList
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

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if(predicate.test(h)) new NonEmptyList(h,tail.filter(predicate))
    else tail.filter(predicate)
    }



  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    new NonEmptyList[B](transformer.transform(h),tail.map(transformer))
  }

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)
  }




object Run extends App {
  val list = new NonEmptyList(1, new NonEmptyList(7,EmptyList))
  val list2 = new NonEmptyList(5,new NonEmptyList(3,EmptyList))
  val evenPredicate = new EvenPredicate
  val evenList = list2.filter(evenPredicate)
  print(evenList.toString)
  val newList = list ++ list2
  val flatList = newList.flatMap(new IncTransformer)
  print(flatList.toString)


  class EvenPredicate extends MyPredicate[Int]{
    override def test(elem: Int): Boolean = elem%2 == 0
  }

  class IncTransformer extends MyTransformer[Int,MyList[Int]]{
    override def transform(elem: Int): MyList[Int] = new NonEmptyList[Int](elem,new NonEmptyList(elem+1,EmptyList))
  }





}





