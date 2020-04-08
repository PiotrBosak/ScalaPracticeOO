package basics

object Exceptions extends App {

  def throwException(): String = throw new RuntimeException


  val potentialFail = try { // potentialFail is AnyVal because it's either String or Unit, now its string
    throwException()
  }
  catch {
    case e: NullPointerException => "pedal"
    case f: RuntimeException => "tezPedal"
  }
  finally print("twoj stary to pedal")


  class MyException extends Exception

  val exception = new MyException
  //1/crash with SOError
  //3. pocket calc
  //add(x,y)
  //subtract
  //multiply
  //divide(by zero exception
  //Throw exception if exceeds In.Max_Value
  //Underflow Exception MInValue
  //divideByZero(Math calc exception


  def fact(n: Int): Int =
    if(n <= 1) n
    else  n * fact(n-1)

  case object MyCalculator{
     def add(x:Int,y:Int): Int = {
       x+y
     }
    def subtract(x:Int,y:Int): Int  = x-y


  }
}