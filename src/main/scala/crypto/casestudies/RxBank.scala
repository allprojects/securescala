// https://github.com/samuelgruetter/rx-playground
// https://github.com/ReactiveX/RxScala/blob/4857c7727d0fb2192a95740e545a7b69785f08b4/examples/src/test/scala/examples/Olympics.scala
// https://github.com/ReactiveX/RxJava/wiki/Additional-Reading
package crypto.casestudies

import rx.lang.scala.Observable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._

import crypto._
import crypto.cipher._
import crypto.dsl._
import crypto.dsl.Implicits._

import scalaz._

object Hello {
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

case class Transaction(sender: EncString, receiver: EncString, amount: EncInt)

object Transaction {
  val senders = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
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
