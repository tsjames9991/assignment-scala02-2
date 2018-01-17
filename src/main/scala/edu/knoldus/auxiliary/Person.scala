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
          case `one` => if (times == 3) "Won" else playGame(times + 1, Random.nextInt(six))
          case `six` => if (times == 3) "Won" else playGame(times + 1, Random.nextInt(six))
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
      val maxTuple:(String, Int) = blogList.maxBy(_._2)
      s"Favourite Blog of $name is ${maxTuple._1}. Number of blogs written is ${maxTuple._2}"
    }
  }
}

object Operations extends App {
  val obj = new Operations
  val log = Logger.getLogger(this.getClass)
  val trainer = obj.Trainer("Sudeep James Tirkey")
  log.info(trainer.personTask())
}