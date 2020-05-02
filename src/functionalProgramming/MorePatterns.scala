package functionalProgramming

import exercises.{EmptyList, MyList, NonEmptyList}

object MorePatterns extends App {
  //1 - constants
  val x: Any = "Scala"
  val constants = x match {
    case 1 => "a number"
    case "ss" => "fj;kla"
    case true => "fjl;as"
    case MorePatterns => " a singleton object"

  }

  //2 match anything
  val matchAnythin = x match {
    case _ => "asdf"
  }

  //2.2 variable
  val matchVariable = x match {
    case something => s"I've found $something"
  }

  //3. tuples
  val aTuple = 1 -> 3
  val matchAtuple = aTuple match {
    case (1,1) => "osm"
    case (something,2) => "fjklas"

  }
  val nestedTuple = (1, (4,3))
  val matchNested = nestedTuple match {
    case (_, (2,v)) => v
  }
  //can be nested
  //4 case classes - constructor pattern
    // can be nested with case classes as well

  val aList: MyList[Int] = NonEmptyList(1,NonEmptyList(1,EmptyList))
  val matchAlist = aList match {
    case EmptyList => "twoj stary"
    case NonEmptyList(1,EmptyList) => "stomething"
    case NonEmptyList(_,_) => "jfkdkljfdsfdsajk"
  }

  //5. list patterns
  val aStandardList = List(1,2,3,5)
  val standardListMatching = aStandardList match {
    case List(1,_,_,_) => ///extractor
    case List(1,_*) => "raz"
    case 1 :: List(_) => "fja;ks"
    case List(1,2,3) :+ 32 => //infix pattern

  }

  //6 type specifiers
  val unknown: Any = 2
  val unknownMatch = unknown match {
    case lsit: List[Int] =>
    case int: Int =>
  }

  //7 name binding
  val nameBindigngMatch = aList match {
    case nazwa @ NonEmptyList(1,EmptyList) => nazwa.t
    case NonEmptyList(1, rest @ NonEmptyList(2,_)) => rest
  }

  // 8 multiPatterns
  val multiPattern = aList match {
    case EmptyList | NonEmptyList(0, _) =>


      //9 if guards
       val matche = aList match {
         case NonEmptyList(1,superElement @ NonEmptyList(1,EmptyList)) if superElement.h %2 == 0 => "twoj stary"
       }
  }

  //all.

  /*
  questions

   */
//type erasure, first one is matched because of how JVM works
  val numbers = List(1,2,3)
  val numbersMatch = numbers match {
    case listOfStrings: List [String] => "a list of strings"
    case listOfNumbers: List[Int] => " a list of numbers"
    case _ => ""

  }



}
