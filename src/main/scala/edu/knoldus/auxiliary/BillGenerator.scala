package edu.knoldus.auxiliary

import org.apache.log4j.Logger

class BillGenerator {

  abstract class TiffinService(payment: Double = 0)

  case class Paytm(payment: Double) extends TiffinService(payment)

  case class NetBanking(payment: Double) extends TiffinService(payment)

  case class CardPayment(payment: Double) extends TiffinService(payment)

  case class Cash(payment: Double) extends TiffinService(payment)

  def printBill(tf: TiffinService): Double = {
    tf match {
      case Paytm(payment: Double) => payment + ((payment * 2)/100)
      case NetBanking(payment: Double) => payment + 5
      case CardPayment(payment: Double) => payment + 1.5
      case Cash(payment: Double) => payment
    }
  }
}

object BillGenerator extends App {
  val obj = new BillGenerator
  val billAmount: Double = 105
  val log = Logger.getLogger(this.getClass)
  log.info("\nUsing PayTM\n")
  log.info(s"${obj.printBill(obj.Paytm(billAmount))}")
  log.info("\nUsing Net Banking\n")
  log.info(s"${obj.printBill(obj.NetBanking(billAmount))}")
  log.info("\nUsing Card Payment\n")
  log.info(s"${obj.printBill(obj.CardPayment(billAmount))}")
  log.info("\nUsing Cash\n")
  log.info(s"${obj.printBill(obj.Cash(billAmount))}")
}
