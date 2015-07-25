// https://github.com/samuelgruetter/rx-playground
// https://github.com/ReactiveX/RxScala/blob/4857c7727d0fb2192a95740e545a7b69785f08b4/examples/src/test/scala/examples/Olympics.scala
// https://github.com/ReactiveX/RxJava/wiki/Additional-Reading
package crypto.casestudies

import java.awt.Color
import java.awt.Insets
import rx.lang.scala.Observable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing._
import scala.util._
import scalaz.syntax.applicative._
// import scalaz.syntax.traverse._

import crypto._
import crypto.cipher._
import crypto.dsl._
import crypto.dsl.Implicits._

import scalaz._

object Hello {
  import RxBankConstants._

  val interp = LocalInterpreter.create
  implicit val keyRing = interp.keyRing

  def main(args: Array[String]) = {
    val rand = new util.Random
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
  private var storage: OpeString ==>> EncInt = ==>>[OpeString,EncInt]()
  def process: Transaction => Unit = { case Transaction(sender,receiver,amount) =>
    val (senderOpe,receiverOpe) = interp {
      toOpeStr(sender) tuple toOpeStr(receiver)
    }

    val (newSenderAmount,newReceiverAmount) = interp {
      (storage.lookup(senderOpe).getOrElse(zero)-amount) tuple
      (storage.lookup(receiverOpe).getOrElse(zero)+amount)
    }

    storage = storage.insert(senderOpe,newSenderAmount)
    storage = storage.insert(receiverOpe,newReceiverAmount)
  }

  def print(implicit K: KeyRing): Unit = {
    storage.toList.foreach { case (name,amount) =>
      println(Common.decryptStr(K)(name) + ": " + Common.decrypt(K)(amount))
    }
  }
}

object RxBankGui extends SimpleSwingApplication {
  import RxBankConstants._

  private def newField = new TextField {
    text = "0"
    columns = 10
    editable = false
  }

  private val fields = List.fill(senders.size)(newField)
  val colors = List.iterate(java.awt.Color.RED, senders.size)(_.brighter)
  val fieldFor = senders.zip(fields).toMap

  def top = new MainFrame {
    title = "RxBank"
    contents = new BoxPanel(Orientation.Vertical) {
      senders.zipWithIndex.foreach { case (s,i) =>
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Label(f"${s}%s:")
          contents += fieldFor(s)
        }
      }
      contents += new FlowPanel (new ScrollPane (new ListView(List())))
    }
  }
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

  val colorMapping: Map[Int,Color]= List(
    (-100, new Color(215, 0, 14)),
    (-50, new Color(247, 0, 14)),
    (0, new Color(255, 128, 14)),
    (50, new Color(87, 255, 69)),
    (100, new Color(0, 224, 14))
  ).toMap

  case class Transaction(sender: EncString, receiver: EncString, amount: EncInt)

  object Transaction {

    def rand(rand: Random)(implicit K: KeyRing): Transaction = {
      val index = rand.nextInt(senders.length)
      val sender = senders(index)

      val remaining = senders.diff(List(sender))
      val index2 = rand.nextInt(remaining.length)
      val receiver = remaining(index2)

      val amount = rand.nextInt(999)

      Transaction(
        Common.encryptStrAes(K)(sender.toString),
        Common.encryptStrAes(K)(receiver.toString),
        Common.encrypt(Additive,K)(amount)
      )
    }
  }

}
