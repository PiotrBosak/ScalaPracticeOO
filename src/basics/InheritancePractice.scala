package basics

object InheritancePractice extends App{

  class Animal {

    val numberOfLegs = 2;
    def eat = println("nomnom")
    private def shit = print("shitting")
    protected def walk = print("walking")
  }

  class Cat extends Animal{
    override def walk: Unit = print("cat walking")
  }

  val cat = new Cat
  cat.eat
  //cat.shit is not possible
  //cat.walk also not
  cat.walk


  //////////////////////////////////////

  class Person(name:String, age:Int){
    def this(name:String) = this(name,0)
    def this() = this("john")
  }

  class Adult(name:String,age:Int,idCard: String) extends Person(name,age)// we need to use constructor



  class Dog(override val numberOfLegs: Int) extends Animal

  val dog = new Dog(4)

    //type substitution



}
