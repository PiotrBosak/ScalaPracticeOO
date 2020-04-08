package basics

object CaseClasses extends App{

  case class Person(name:String, age:Int)
  val a = new Person("a",3)

  val aCopied = a.copy("john")
  // they have companoin objects
  val mary = Person("Mary",322)// companion object apply method
  //case classes are seriazibale

  //ccs have extractor patterns, can be used in pattern matching


}

case object USA{
  def name: String = "United States"
}
