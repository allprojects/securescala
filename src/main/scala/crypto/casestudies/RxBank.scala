package crypto.casestudies

import java.awt.Color
import java.awt.Insets
import rx.lang.scala._
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
import crypto.dsl.Implicits._
import crypto.dsl._
// import crypto.remote._

case class Transaction(sender: String, receiver: String, amount: EncInt)

object Transaction {
  import RxBankConstants._
  private val cache = collection.mutable.Map[Int,EncInt]()

  def rand(rand: Random)(implicit K: KeyRing): Transaction = {
    val index = rand.nextInt(senders.length)
    val sender = senders(index)

    val remaining = senders.diff(List(sender))
    val index2 = rand.nextInt(remaining.length)
    val receiver = remaining(index2)

    val amount = rand.nextInt(20) + 5
    val encAmount = cache.getOrElseUpdate(amount, Common.encrypt(Additive,K)(amount))

    Transaction(
      sender,
      receiver,
      encAmount
    )
  }
}

class RxBank(ts: Observable[Transaction])(interp: PureCryptoInterpreter, zero: EncInt) {
  private var storage: Map[String,EncInt] = Map()

  private val subjUpdates: Subject[(String,EncInt)] = Subject()
  val updates: Observable[(String,EncInt)] = subjUpdates

  ts.doOnTerminate(subjUpdates.onCompleted).foreach(process)

  private def process: Transaction => Unit = { case Transaction(sender,receiver,amount) =>
    val (newSenderAmount,newReceiverAmount) = interp {
      (storage.get(sender).getOrElse(zero)-amount) tuple
      (storage.get(receiver).getOrElse(zero)+amount)
    }

    storage = storage + (sender -> newSenderAmount)
    storage = storage + (receiver -> newReceiverAmount)

    subjUpdates.onNext((sender,newSenderAmount))
    subjUpdates.onNext((receiver,newReceiverAmount))
  }

  def balance: String => EncInt = s => storage.get(s).getOrElse(zero)
}

class RxBankInterface(ts: Observable[Transaction])(
  updates: Observable[(String,EncInt)],
  interp: PureCryptoInterpreter
) extends SimpleSwingApplication { gui =>

  import RxBankConstants._

  val encColorMapping: List[(EncInt,Color)] = colorMapping.map { case (thresh,c) => (interp(encrypt(Comparable)(thresh)),c)}

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

  def title = "RxBank Encrypted"

  def top = new MainFrame {
    title = gui.title
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

  ts.foreach(logTransaction)

  def logTransaction(t: Transaction) = t match {
    case Transaction(sender,receiver,amnt) =>
      val old = messages.listData
      messages.listData = s"${sender} -> ${receiver}" +: old
  }

  updates.doOnTerminate(quit()).foreach { case (account,newAmount) => redrawFor(account,newAmount)}

  private def redrawFor(s: String, amt: EncInt): Unit = {
    val field = fieldFor(s)
    val colors = interp { encColorMapping.traverse { case (t,c) => (amt > t).map((_,c)) } }.filter(_._1)
    if (colors.nonEmpty)
      field.background = colors.last._2
  }
}

class RxBankInterfaceDecrypted(ts: Observable[Transaction])(
  updates: Observable[(String,EncInt)],
  interp: PureCryptoInterpreter, keyRing: KeyRing
) extends RxBankInterface(ts)(updates,interp) {

  override def title = "RxBank Decrypted"

  override def logTransaction(t: Transaction) = t match {
    case Transaction(sender,receiver,amnt) =>
      val old = messages.listData
      messages.listData = s"${sender} -> ${receiver} (${Common.decrypt(keyRing)(amnt)})" +: old
  }

  updates.doOnTerminate(quit()).foreach { case (account,newAmount) => redrawFor(account,newAmount) }

  private def redrawFor(account: String, amt: EncInt): Unit = {
    val field = fieldFor(account)
    val colors = interp { encColorMapping.traverse { case (t,c) => (amt > t).map((_,c)) } }.filter(_._1)
    field.text = Common.decrypt(keyRing)(amt).toString
    if (colors.nonEmpty)
      field.background = colors.last._2
  }
}

case class PlainTransaction(sender: String, receiver: String, amount: Int)
object PlainTransaction {
  import RxBankConstants._

  def rand(rand: Random): PlainTransaction = {
    val index = rand.nextInt(senders.length)
    val sender = senders(index)

    val remaining = senders.diff(List(sender))
    val index2 = rand.nextInt(remaining.length)
    val receiver = remaining(index2)

    val amount = rand.nextInt(20) + 5

    PlainTransaction(
      sender,
      receiver,
      amount
    )
  }
}

class RxBankInterfacePlain(ts: Observable[PlainTransaction])(
  updates: Observable[(String,Int)]) extends SimpleSwingApplication { gui =>

  import RxBankConstants._

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

  def title = "RxBank Plain"

  def top = new MainFrame {
    title = gui.title
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

  ts.foreach(logTransaction)

  def logTransaction(t: PlainTransaction) = t match {
    case PlainTransaction(sender,receiver,amnt) =>
      val old = messages.listData
      messages.listData = s"${sender} -> ${receiver} (${amnt})" +: old
  }

  updates.doOnTerminate(quit()).foreach { case (account,newAmount) => redrawFor(account,newAmount)}

  private def redrawFor(s: String, amt: Int): Unit = {
    val field = fieldFor(s)
    val colors = colorMapping.filter { case (t,c) => amt > t }
    field.text = amt.toString
    if (colors.nonEmpty)
      field.background = colors.last._2
  }
}

class RxBankPlain(ts: Observable[PlainTransaction]) {
  private var storage: Map[String,Int] = Map()

  private val subjUpdates: Subject[(String,Int)] = Subject()
  val updates: Observable[(String,Int)] = subjUpdates

  ts.doOnTerminate(subjUpdates.onCompleted).foreach(process)

  private def process: PlainTransaction => Unit = {
    case PlainTransaction(sender,receiver,amount) =>

      val newSenderAmount = storage.get(sender).getOrElse(0) - amount
      val newReceiverAmount = storage.get(receiver).getOrElse(0) + amount

      storage = storage + (sender -> newSenderAmount)
      storage = storage + (receiver -> newReceiverAmount)

      subjUpdates.onNext((sender,newSenderAmount))
      subjUpdates.onNext((receiver,newReceiverAmount))
  }

  def balance: String => Int = s => storage.get(s).getOrElse(0)
}

object RxBankApp extends App {
  implicit val keyRing = KeyRing.create

  val NUM_TRANSACTIONS = 100

  // val interp = new LocalInterpreter(keyRing)

  // val globalEC = scala.concurrent.ExecutionContext.Implicits.global
  // val cryptoService = new CryptoServiceImpl(keyRing)(globalEC)
  // val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  // val interp = Blocking {
  //   new RemoteInterpreterOptAnalyze(cryptoService, pubKeys, FixedBatch(10), _ > 3)(globalEC)
  // }

  // val rand1 = new Random
  // var ts = Observable.from(List.fill(NUM_TRANSACTIONS)(Transaction.rand(rand1))).publish
  // val bank = new RxBank(ts)(interp, Common.zero(keyRing))

  // val enc = Future { new RxBankInterface(ts)(bank.updates,interp).startup(Array()) }

  // val dec = Future {new RxBankInterfaceDecrypted(ts)(bank.updates,interp,keyRing).startup(Array())}

  val rand2 = new Random
  val tsPlain =
    Observable.from(List.fill(NUM_TRANSACTIONS)(PlainTransaction.rand(rand2))).publish
  val bankPlain = new RxBankPlain(tsPlain)

  val plain = Future { new RxBankInterfacePlain(tsPlain)(bankPlain.updates).startup(Array()) }

  Thread.sleep(1000)

  // ts.connect
  // Await.result(Future.sequence(List(enc)),30.minutes)

  tsPlain.connect
  Await.result(plain,30.minutes)
}

object RxBankConstants {
  import java.awt.Color
  val senders = List(
    "Jancis",
    "Caroline",
    "Denton",
    "Berny",
    "Joslyn",
    "Emma",
    "Kaitlyn",
    "Claudia",
    "Breanna",
    "Aurora"
  )

  val colorMapping: List[(Int,Color)]= List(
    (-100, new Color(215, 0, 14)),
    (-50, new Color(247, 0, 14)),
    (0, new Color(255, 128, 14)),
    (50, new Color(87, 255, 69)),
    (100, new Color(0, 224, 14))
  )
}
