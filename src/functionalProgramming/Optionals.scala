package functionalProgramming

import scala.util.Random

object Optionals extends App {

  val myOption: Option[Int] = Some(4)
  val noOption : Option[Int] = None

  val config: Map[String,String] = Map(
    "host" -> "17,3,42.423",
    "port" -> "fjsakljkfsad"
  )
  class Connection{
    def connect = "connected"
  }
  object Connection{
    val random = new Random(System.nanoTime())
    def apply(host:String, port:String) : Option[Connection] =
      if(random.nextBoolean()) Some(new Connection)
      else None
  }

  val host: Option[String] = config.get("host")
  val port : Option[String] =  config.get("port")
  val myConnection = host.map(h => port.flatMap( p => Connection(h,p)))
 val myConnection2 = for {
   h <- config.get("host")
   p <- config.get("port")
   c <- Connection(h,p)
 }yield c.connect
  myConnection2.foreach(println)
}
