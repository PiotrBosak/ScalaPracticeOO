package functionalProgramming

object WhatsAFunction extends App {

  val stringToIntConverter: String => Int = e => e.toInt

  stringToIntConverter("3")


  //takes to string and concat
  //MyList and transform myPredicate and transfomer into function types
  //def function which takes an int and return a function whi`ch takes int and returns int

  val concat: (String, String) => String = (e,f) => e.concat(f)

  val fancyFunction : Int => (Int => Int) = (e => (x => x*e))
  val first = fancyFunction(3)(5)// curried function
  println(first)


  def toCurry(f: (Int,Int) => Int): (Int => Int => Int) =
    x => y => f(x,y)

  def fromCurry(f: (Int => Int => Int)) : (Int,Int) => Int =
    (x,y) => f(x)(y)
  def compose(f: Int => Int,g: Int=> Int): Int => Int =
     x => f(g(x))

}






