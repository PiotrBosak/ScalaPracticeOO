package PatternMatching

import scala.util.Random

object PatternMatchingBasics extends App {
  val random = new Random
  val x = random.nextInt(10)
  val desc = x match {
    case 1 => "one"
    case 2 => "two"
    case _ => "something else"
  }
  println(desc)

  //1. can decompose valeus
  //used with case classes

  case class Person(name: String, age: Int)

  val bob = Person("Bob", 20)

  val greeting = bob match {
    case Person(a, b) => s"I'm $a and I'm $b"
    case _ => "I dont know who I am"
  }
  println(greeting)
  val greeting2 = bob match {
    case Person(a, b) if b > 21 => s"I'm $a and I'm $b"
    case _ => "I dont know who I am"
  }

  println(greeting2)

  //1. cases are matched in order
  //type of pattern match is unification of all cases
  //Pattern matching on sealed hierarchies
  // works well with case classes


  sealed class Animal

  case class Dog(breed: String) extends Animal

  case class Parrot(greeting: String) extends Animal

  val animal: Animal = Dog("a")
  animal match {
    case Dog(someBreed) => println(s"matched a dog $someBreed")

  }
  /*
  patternMatching uses PM
  takes

  sum(number(1), number(2)) => 1+2
   */
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr,e2:Expr)extends Expr
  case class Prod(e1:Expr, e2:Expr) extends Expr

  def printExpression(expr: Expr):Unit = {
    expr match {
      case Number(something) => println(something)
      case Sum(e1,e2) =>
        printExpression(e1)
        print("+")
        printExpression(e2)
      case Prod(e1,e2) =>
        printExpression(e1)
        print("*")
        printExpression(e2)
    }
  }

  printExpression(Sum(Number(1),Number(2)))

}
