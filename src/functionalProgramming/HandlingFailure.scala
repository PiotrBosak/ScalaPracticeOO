package functionalProgramming

import scala.util.{Failure, Random, Success, Try}

object HandlingFailure extends App {
  val aSuccess = Success(3)
  val aFailure = Failure(new RuntimeException("failure"))

  def unsafeMethod(): String = throw new RuntimeException

  val potentialFail = Try(unsafeMethod())

  def safeMethod(): String = "cos"

  val anotherFailure = Try {
    throw new RuntimeException
  }
  println(Try(unsafeMethod()).orElse(Try(anotherFailure)))
  val hostname = "localhost"
  val port = "8080"

  def renderHtml(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>"
      else throw new RuntimeException("connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())
    def getConnection(host: String, port : String) : Connection = {
      if(random.nextBoolean()) new Connection
      else throw new RuntimeException("port taken")
    }
  }

  //print
  //if you get the html page from the conneciton, print it to the console. call html

  val potentialConnection : Try[Connection] = Try(HttpService.getConnection(hostname,port))
  val htmlPage : Try[String] = Try(potentialConnection.get.get("some url"))
  htmlPage.foreach(renderHtml)

}
