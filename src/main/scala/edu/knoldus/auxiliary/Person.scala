package edu.knoldus.auxiliary

import org.apache.log4j.Logger

import scala.util.Random

class Operations {
  private val zero: Int = 0
  private val one: Int = 1
  private val six: Int = 6
  private val fifty: Int = 50

  abstract class Person(name: String) {
    def personTask(): String
  }

  case class Gamer(name: String) extends Person(name) {

    override def personTask(): String = {

      def playGame(times: Int, value: Int): String = {
        value match {
          case `one`|`six` => if (times == 3) "Won" else playGame(times + 1, Random.nextInt(six))
          case _ => "Lost"
        }
      }

      s"$name has ${playGame(zero, Random.nextInt(six))}"
    }
  }

  case class Trainer(name: String) extends Person(name) {

    override def personTask(): String = {
      val attendance: Int = Random.nextInt(fifty)
      s"Attendance in $name Class : ${attendance}"
    }
  }

  case class Blogger(name: String, blogList: Map[String, Int]) extends Person(name) {

    override def personTask(): String = {
      val newBlogList = blogList.map { case (k, v) => (k, v + Random.nextInt(six)) }
      val maximum: (String, Int) = newBlogList.maxBy(_._2)
      s"Updated Blog List : ${newBlogList}\nFavourite Blog of $name is  ${maximum._1} \nNumber of posts : ${maximum._2}"
    }
  }

}

object Operations extends App {
  val obj = new Operations
  val initialMap = Map("Scala" -> 2, "Java" -> 5, "Kafka" -> 6)
  val log = Logger.getLogger(this.getClass)
  val gamer = obj.Gamer("Sudeep James Tirkey")
  val trainer = obj.Trainer("Vinay Kumar")
  val blogger = obj.Blogger("Bruce Schneier", initialMap)
  log.info(gamer.personTask())
  log.info("\n\n")
  log.info(trainer.personTask())
  log.info("\n\n")
  log.info(blogger.personTask())
}
