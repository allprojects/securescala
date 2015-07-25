// https://github.com/samuelgruetter/rx-playground
// https://github.com/ReactiveX/RxScala/blob/4857c7727d0fb2192a95740e545a7b69785f08b4/examples/src/test/scala/examples/Olympics.scala
// https://github.com/ReactiveX/RxJava/wiki/Additional-Reading
package crypto.casestudies

import java.awt.Color
import java.awt.Insets
import rx.lang.scala.Observable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing._
import scala.util.Random
import scala.util._
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._

import crypto._
import crypto.cipher._
import crypto.dsl._
import crypto.dsl.Implicits._

object Hello {
  import RxBankConstants._

  val interp = LocalInterpreter.create
  implicit val keyRing = interp.keyRing

  def main(args: Array[String]) = {
    val rand = new Random
    val bank = new RxBank(interp, Common.zero(keyRing))
    Observable.
      interval(50 millis).
      map(_ => Transaction.rand(rand)).
      take(100).
      toBlocking.
      foreach(bank.process)
    bank.print(keyRing)
  }
}

class RxBank(interp: PureCryptoInterpreter, zero: EncInt) {
  import RxBankConstants._
  private var storage: Map[String,EncInt] = Map()
  def process: Transaction => Unit = { case Transaction(sender,receiver,amount) =>
    val (newSenderAmount,newReceiverAmount) = interp {
      (storage.get(sender).getOrElse(zero)-amount) tuple
      (storage.get(receiver).getOrElse(zero)+amount)
    }

    storage = storage + (sender -> newSenderAmount)
    storage = storage + (receiver -> newReceiverAmount)
  }

  def balance: String => EncInt = s => storage.get(s).getOrElse(zero)

  def print(implicit K: KeyRing): Unit = {
    storage.toList.foreach { case (name,amount) =>
      println(name + ": " + Common.decrypt(K)(amount))
    }
  }
}

class RxBankInterfacePlain(model: RxBank, interp: PureCryptoInterpreter, keyRing: KeyRing)
    extends SimpleSwingApplication {

  import RxBankConstants._

  val encColorMapping = colorMapping.map { case (thresh,c) => (interp(encrypt(Comparable)(thresh)),c)}

  private def newField = new TextField {
    text = ""
    columns = 6
    editable = false
  }

  private val fields = List.fill(senders.size)(newField)
  val fieldFor = senders.zip(fields).toMap
  val messages = new ListView[String](List()) {
    preferredSize = new Dimension(300,500)
  }

  def top = new MainFrame {
    title = "RxBank Plain"
    contents = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(400,500)
      senders.zipWithIndex.foreach { case (s,i) =>
        contents += new FlowPanel(FlowPanel.Alignment.Leading)() {
          contents += Swing.HStrut(60)
          contents += fieldFor(s)
          contents += new Label(s)
        }
      }
      contents += new FlowPanel (new ScrollPane (messages))
    }
  }

  def logTransaction(t: Transaction) = t match {
    case Transaction(sender,receiver,amnt) =>
      val old = messages.listData
      messages.listData = s"${sender} -> ${receiver} (${Common.decrypt(keyRing)(amnt)})" +: old
      redrawFor(sender)
      redrawFor(receiver)
  }

  private def redrawFor(s: String): Unit = {
    val field = fieldFor(s)
    val amt = model.balance(s)
    val colors = interp { encColorMapping.traverse { case (t,c) => (amt > t).map((_,c)) } }.filter(_._1)
    field.text = Common.decrypt(keyRing)(amt).toString
    if (colors.nonEmpty)
      field.background = colors.last._2
  }
}

class RxBankInterface(model: RxBank, interp: PureCryptoInterpreter)
    extends SimpleSwingApplication {

  import RxBankConstants._

  val encColorMapping = colorMapping.map { case (thresh,c) => (interp(encrypt(Comparable)(thresh)),c)}

  private def newField = new TextField {
    text = ""
    columns = 6
    editable = false
  }

  private val fields = List.fill(senders.size)(newField)
  val fieldFor = senders.zip(fields).toMap
  val messages = new ListView[String](List()) {
    preferredSize = new Dimension(300,500)
  }

  def top = new MainFrame {
    title = "RxBank Encrypted"
    contents = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(400,500)
      senders.zipWithIndex.foreach { case (s,i) =>
        contents += new FlowPanel(FlowPanel.Alignment.Leading)() {
          contents += Swing.HStrut(60)
          contents += fieldFor(s)
          contents += new Label(s)
        }
      }
      contents += new FlowPanel (new ScrollPane (messages))
    }
  }

  def logTransaction(t: Transaction) = t match {
    case Transaction(sender,receiver,amnt) =>
      val old = messages.listData
      messages.listData = s"${sender} -> ${receiver}" +: old
      redrawFor(sender)
      redrawFor(receiver)
  }

  private def redrawFor(s: String): Unit = {
    val field = fieldFor(s)
    val amt = model.balance(s)
    val colors = interp { encColorMapping.traverse { case (t,c) => (amt > t).map((_,c)) } }.filter(_._1)
    if (colors.nonEmpty)
      field.background = colors.last._2
  }

}

object RxBankApp extends App {
  import RxBankConstants._
  val interp = LocalInterpreter.create
  implicit val keyRing = interp.keyRing

  val bank = new RxBank(interp, Common.zero(keyRing))
  val interface = new RxBankInterface(bank,interp)
  val interfacePlain = new RxBankInterfacePlain(bank,interp,keyRing)

  val rand = new Random

  Observable.
    interval(200 millis).
    map(_ => Transaction.rand(rand)).
    foreach{ t =>
      bank.process(t)
      interface.logTransaction(t)
      interfacePlain.logTransaction(t)
    }

  val i1 = Future { interface.startup(Array()) }
  val i2 = Future { interfacePlain.startup(Array()) }
  val is = Future.sequence(List(i1,i2))
  Await.result(is,Duration.Inf)
}



object RxBankConstants {
  import java.awt.Color
  val senders = List(
    "Rorra",
    "Gugei",
    "Sote",
    "Jaas",
    "Thom",
    "Bapokaz",
    "Ussid",
    "Ethoo",
    "Tattlack",
    "Zhaimta"
  )

  val colorMapping: List[(Int,Color)]= List(
    (-100, new Color(215, 0, 14)),
    (-50, new Color(247, 0, 14)),
    (0, new Color(255, 128, 14)),
    (50, new Color(87, 255, 69)),
    (100, new Color(0, 224, 14))
  )

  case class Transaction(sender: String, receiver: String, amount: EncInt)

  object Transaction {

    def rand(rand: Random)(implicit K: KeyRing): Transaction = {
      val index = rand.nextInt(senders.length)
      val sender = senders(index)

      val remaining = senders.diff(List(sender))
      val index2 = rand.nextInt(remaining.length)
      val receiver = remaining(index2)

      val amount = rand.nextInt(20 + 5)

      Transaction(
        sender,
        receiver,
        Common.encrypt(Additive,K)(amount)
      )
    }
  }

}
