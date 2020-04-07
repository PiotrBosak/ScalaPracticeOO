package basics

object AbstractDataTypes extends App{

abstract sealed class Animal {
  val creatureType :String
  def eat: Unit
}

  //val animal = new Animal not possible
  class Dog extends Animal with Carnivore {

    override val creatureType: String = "domestic"
    override def eat: Unit = print("nom nom")

    override def eat(animal: Animal): Unit = print(s"I'm a croc and I'm eating a ${animal.creatureType}");
  }


  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  val dog = new Dog
  dog.eat(new Dog)


}
