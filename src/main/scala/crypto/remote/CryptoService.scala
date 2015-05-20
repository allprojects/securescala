package crypto.remote

import scala.concurrent._
import scala.concurrent.duration._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import com.typesafe.config.ConfigFactory

import crypto._
import crypto.cipher._

trait CryptoService {
  /** Replies with the public keys of the KeyRing used by this service */
  def publicKeys: Future[PubKeys]

  def toPaillier(in: Enc): Future[PaillierEnc]
  def toElGamal(in: Enc): Future[ElGamalEnc]
  def toAes(in: Enc): Future[AesEnc]
  def toOpe(in: Enc): Future[OpeEnc]

  /** Convert the encoded value for the given scheme */
  def convert(s: Scheme)(in: Enc): Future[Enc]
  /** Process the list of (scheme,encoding) and convert the encoding to
    * scheme (if necessary) before replying with the whole list of
    * results
    */
  def batchConvert(xs: List[(Scheme,Enc)]): Future[List[Enc]]

  /** Encrypt the plain number with the given scheme NOTE: the value is
    * NOT encrypted for sending and therefore may be visible to
    * others!  If possible you should use the public keys and encrypt
    * them with an asymmetric scheme like paillier or elgamal before
    * sending it.
    */
  def encrypt(s: Scheme)(in: Int): Future[Enc]
  /** Process the list of (scheme,integer) and encrypt them, reply after
    * the whole list is processed
    */
  def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[Enc]]

  /** Decrypt the value and print it locally (where the service runs) to stdout */
  def decryptAndPrint(v: Enc): Unit

  /** Print the string on the CrytpoService side */
  def println[A](a: A): Unit
}

trait CryptoServicePlus extends CryptoService {
  def subtract(lhs: Enc, rhs: Enc): Future[Enc]
  def integerDivide(lhs: Enc, rhs: Enc): Future[Enc]
}

class CryptoServiceImpl(keyRing: KeyRing) extends CryptoService with CryptoServicePlus {
  private def doConvert(s: Scheme, in: Enc) = Common.depConvert(keyRing)(s,in)
  private def additive(x: Enc): PaillierEnc = doConvert(Additive, x)
  private def multiplicative(x: Enc): ElGamalEnc = doConvert(Multiplicative, x)
  private def equality(x: Enc): AesEnc = doConvert(Equality, x)
  private def comparable(x: Enc): OpeEnc = doConvert(Comparable, x)

  override def publicKeys = Future.successful(keyRing.pub)

  override def toPaillier(in: Enc): Future[PaillierEnc] = Future.successful(additive(in))
  override def toElGamal(in: Enc): Future[ElGamalEnc] =
    Future.successful(multiplicative(in))
  override def toAes(in: Enc): Future[AesEnc] = Future.successful(equality(in))
  override def toOpe(in: Enc): Future[OpeEnc] = Future.successful(comparable(in))

  override def convert(s: Scheme)(in: Enc): Future[Enc] =
    Future.successful(Common.convert(keyRing)(s,in))
  override def batchConvert(xs: List[(Scheme,Enc)]): Future[List[Enc]] =
    Future.successful(xs.map { case (s, enc) => Common.convert(keyRing)(s,enc)})

  override def encrypt(s: Scheme)(in: Int): Future[Enc] =
    Future.successful(Common.encrypt(s, keyRing)(in))
  override def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[Enc]] =
    Future.successful(xs.map { case (s, i) => Common.encrypt(s, keyRing)(i)})

  override def decryptAndPrint(v: Enc): Unit = println(Common.decrypt(keyRing.priv)(v))
  override def println[A](a: A): Unit = Predef.println(a)

  override def integerDivide(lhs: Enc, rhs: Enc): Future[Enc] = Future.successful {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs / plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }

  override def subtract(lhs: Enc, rhs: Enc): Future[Enc] = Future.successful {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs - plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }
}

object CryptoService {
  def start: (ActorSystem, CryptoServicePlus) =
    startWith(4242, KeyRing.create, "cryptoService")
  def startWith(port: Int, keyRing: KeyRing, name: String):
      (ActorSystem, CryptoServicePlus) = {

    val config = ConfigFactory.parseString(s"""
akka {
  loglevel = "ERROR"
  stdout-loglevel = "ERROR"
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = ${port}
      maximum-frame-size = 256000b
    }
  }
}
""")

    val system = ActorSystem("CryptoService", config)

    val cryptoService: CryptoServicePlus =
      TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus],
        new CryptoServiceImpl(keyRing)), name)

    (system, cryptoService)
  }

  def connect(implicit ec: ExecutionContext) =
    connectWith("127.0.0.1", 4242, "cryptoService")

  def connectWith(address: String, port: Int, name: String)(
    implicit ec: ExecutionContext): (ActorSystem, Future[CryptoServicePlus]) = {

    val config = ConfigFactory.parseString("""
akka {
  loglevel = "ERROR"
  stdout-loglevel = "ERROR"
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = 0
      maximum-frame-size = 256000b
    }
  }
}
""")

    val system = ActorSystem("CryptoServiceClient", config)

    implicit val timeOut = Timeout(60.seconds)
    val futureRef =
      system.actorSelection(s"akka.tcp://CryptoService@${address}:${port}/user/${name}").
        resolveOne().map { ref =>
          TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus]).
            withTimeout(timeOut), ref)
        }
    (system, futureRef)
  }
}

object StartCryptoService extends App {
  val (system, service) = CryptoService.start
  println("CryptoService is up and running.")
  System.in.read()
  print("Shutting down...")
  system.shutdown()
  println("bye!")
}
