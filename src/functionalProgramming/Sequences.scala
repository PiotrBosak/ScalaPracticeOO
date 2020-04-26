package functionalProgramming

import scala.util.Random

object Sequences extends App {
  val aSequence = Seq(1, 2, 3, 4)
  print(aSequence)
  val newSeq = aSequence.flatMap(e => Seq(e, e * 2))
  println(newSeq)
  val list1 = Seq(1, 2, 3)
  val list2 = Seq(1, 2, 3)
  val list3 = Seq(1, 2, 3)

  val combinations = list1.flatMap(e => list2.flatMap(f => list3.map(g => e * f * g))).distinct
  println(combinations)
  val aRange: Seq[Int] = 1 until 10
  aRange.foreach(x => println(x))
  val aList = List(5, 4)
  val prepended = 42 +: aList :+ 88
  val apples5 = List.fill(5)(if (Random.nextBoolean()) "apple" else "orange")
  println(apples5)
  val newPairing = "pedal" -> 3

  /*
  2. overly simplified social network(maps)
  Person string
  add a person
  remove
  friend
  unfriend

  -number of friends
  -person with most friends
  -how many people have no friends
  -if there is a social connection between two people(direct or not)
   */


  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], friend1: String, friend2: String): Map[String, Set[String]] = {
    val friends1 = network(friend1)
    val friends2 = network(friend2)
    network + (friend1 -> (friends1 + friend2)) + (friend2 -> (friends2 + friend1))
  }

  def unfriend(network: Map[String, Set[String]], friend1: String, friend2: String): Map[String, Set[String]] = {
    val friendsOf1 = network(friend1)
    val friends2 = network(friend2)
    network + (friend1 -> (friendsOf1 - friend2)) + (friend2 -> (friends2 - friend1))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    @scala.annotation.tailrec
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))

    val unfriended = removeAux(network(person), network)
    unfriended - person

  }

  def numberOfFriends(network: Map[String, Set[String]], person: String) =
    if(!network.contains((person))) 0
    else network(person).size

  def numberOfFriends(network: Map[String, Set[String]]) =
    network.size

  def personWithMostFriends(network: Map[String, Set[String]]) = {
    @scala.annotation.tailrec
    def aux(networkAcc: Map[String, Set[String]], maxPerson: String): String = {
      if (networkAcc.isEmpty) maxPerson
      else {
        val currentPerson = networkAcc.head._1
        val newMax: String = if (network(currentPerson).size > network(maxPerson).size) currentPerson else maxPerson
        aux(networkAcc.tail, newMax)
      }

    }

    aux(network, network.head._1)
  }

  def peopleWithNoFriends(network: Map[String, Set[String]]) = {
    @scala.annotation.tailrec
    def aux(networkAcc: Map[String, Set[String]], counter: Int) : Int =
      if (networkAcc.isEmpty) counter
      else {
        val isCurrentPersonFriendless = if(networkAcc.head._2.isEmpty) 1 else 0
        aux(networkAcc.tail,counter+isCurrentPersonFriendless)
      }
    aux(network,0)
  }
  def wayEasierPeopleWithNoFriends(network: Map[String,Set[String]]) =
    network.maxBy(pair => pair._2.size)._1

  def pwnf(network: Map[String,Set[String]]) =
    network.count(pair => pair._2.isEmpty)
  

  val myNetwork: Map[String, Set[String]] = Map("Jim" -> Set("Mary"), "Mary" -> Set("Jim", "Joseph"), "Joseph" -> Set("Mary"))
  println(personWithMostFriends(myNetwork))

  val myNetwork2: Map[String, Set[String]] = Map("Jim" -> Set(), "Mary" -> Set(), "Joseph" -> Set())
  println(peopleWithNoFriends(myNetwork2))



}
