package basics

import basics.Generics.Animal

object Generics extends App {

  class MyList[+A] {
    // use A inside class definition
    // traits can have generics
    def add[B >:A] (element: B) : MyList[B] = ???

  }

  val listOfInt = new MyList[Int]
  val listOfStrings = new MyList[String]

  object MyList {
    def empty[A]: MyList[A] = ???
  }

  val emptyList = MyList.empty[Int]

  //variance problem

  class Animal

  class Dog extends Animal

  class Cat extends Animal

  //covariance,
  class CovariantList[+A]

  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  //can we add a dog to animals

  //2. No

  class InvariantList[A] {

  }

  //3. reverse

  class ContraVariantList[-A] {

  }

  val list: ContraVariantList[Cat] = new ContraVariantList[Animal]

//upper bounded type
  class Cage[A <: Animal](animal: Animal)

  val cage = new Cage(new Dog)

  //lower bound
  class CatCage[A >: Cat]
  val cat = new CatCage[Animal]

}