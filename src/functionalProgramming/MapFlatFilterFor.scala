package functionalProgramming

object MapFlatFilterFor extends App {
  val list = List(1, 2, 3)
  val toPair = (x: Int) => List(x, x + 1)
  val flatList = list.flatMap(toPair)
  //print all combinantion between 2 lists
  val list2 = List(1, 2, 3, 4)
  val list3 = List('a', 'b')
  val list4 = List('z', 'x')
  val newList = list2.flatMap(n => list3.flatMap(c => list4.map(z => "" + n + c + z)))
  print(newList)
  //for comprehensions

  val forCombinations = for {
    n <- list2 if n%2 == 0
    b <- list3
    v <- list4}
    yield n+b+v

print(forCombinations)


}
