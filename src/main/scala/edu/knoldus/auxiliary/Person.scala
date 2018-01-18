package edu.knoldus.auxiliary

import org.apache.log4j.Logger

import scala.util.Random

class Operations {
  private val ZERO: Int = 0
  private val ONE: Int = 1
  private val SIX: Int = 6
  private val FIFTY: Int = 50

  abstract class Person(name: String) {
    def personTask(): String
  }

  case class Gamer(name: String) extends Person(name) {

    override def personTask(): String = {

      def playGame(times: Int, value: Int): String = {
        value match {
          case ONE | SIX => if (times == 3) "Won" else playGame(times + 1, Random.nextInt(SIX) + 1)
          case _ => "Lost"
        }
      }

      s"$name has ${playGame(ZERO, Random.nextInt(SIX) + 1)}"
    }
  }

  case class Trainer(name: String) extends Person(name) {

    override def personTask(): String = {
      val attendance: Int = Random.nextInt(FIFTY)
      s"Attendance in $name Class : ${attendance}"
    }
  }

  case class Blogger(name: String, blogList: Map[String, Int]) extends Person(name) {

    override def personTask(): String = {

      val newBlogList = blogList.map { case (k, v) => (k, v + Random.nextInt(SIX)) }
      val favouriteTech: (String, Int) = newBlogList.maxBy(_._2)
      s"Updated Blog List : ${newBlogList}\nFavourite Blog of $name is  ${favouriteTech._1} \nNumber of posts : ${favouriteTech._2}"
    }
  }
}

object Operations extends App {
  val operation = new Operations
  val initialMap = Map("Scala" -> 2, "Java" -> 5, "Kafka" -> 6)
  val log = Logger.getLogger(this.getClass)
  val gamer = operation.Gamer("Sudeep James Tirkey")
  val trainer = operation.Trainer("Vinay Kumar")
  val blogger = operation.Blogger("Bruce Schneier", initialMap)
  log.info(gamer.personTask())
  log.info("\n\n")
  log.info(trainer.personTask())
  log.info("\n\n")
  log.info(blogger.personTask())
}
