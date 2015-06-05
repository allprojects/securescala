package crypto.remote

import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._

import akka.actor._
import akka.routing._
import akka.pattern.ask
import akka.util.Timeout

import com.typesafe.config.ConfigFactory

import crypto._
import crypto.cipher._

trait CryptoService {
  /** Replies with the public keys of the KeyRing used by this service */
  def publicKeys: Future[PubKeys]

  def toPaillier(in: EncInt): Future[PaillierEnc]
  def toElGamal(in: EncInt): Future[ElGamalEnc]
  def toAes(in: EncInt): Future[AesEnc]
  def toOpe(in: EncInt): Future[OpeEnc]

  /** Convert the encoded value for the given scheme */
  def convert(s: Scheme)(in: EncInt): Future[EncInt]
  /** Process the list of (scheme,encoding) and convert the encoding to
    * scheme (if necessary) before replying with the whole list of
    * results
    */
  def batchConvert(xs: List[(Scheme,EncInt)]): Future[List[EncInt]]

  /** Encrypt the plain number with the given scheme NOTE: the value is
    * NOT encrypted for sending and therefore may be visible to
    * others!  If possible you should use the public keys and encrypt
    * them with an asymmetric scheme like paillier or elgamal before
    * sending it.
    */
  def encrypt(s: Scheme)(in: Int): Future[EncInt]
  /** Process the list of (scheme,integer) and encrypt them, reply after
    * the whole list is processed
    */
  def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[EncInt]]

  /** Decrypt the value and print it locally (where the service runs) to stdout */
  def decryptAndPrint(v: EncInt): Unit

  /** Print the string on the CrytpoService side */
  def println[A](a: A): Unit
}

trait CryptoServicePlus extends CryptoService {
  def subtract(lhs: EncInt, rhs: EncInt): Future[EncInt]
  def integerDivide(lhs: EncInt, rhs: EncInt): Future[EncInt]
  def isEven(enc: EncInt): Future[Boolean]
  def isOdd(enc: EncInt): Future[Boolean]
}

class CryptoServiceImpl(keyRing: KeyRing)(implicit ec: ExecutionContext)
    extends CryptoService with CryptoServicePlus {

  private def doConvert(s: Scheme, in: EncInt) = Common.depConvert(keyRing)(s,in)
  private def additive(x: EncInt): PaillierEnc = doConvert(Additive, x)
  private def multiplicative(x: EncInt): ElGamalEnc = doConvert(Multiplicative, x)
  private def equality(x: EncInt): AesEnc = doConvert(Equality, x)
  private def comparable(x: EncInt): OpeEnc = doConvert(Comparable, x)

  def wrap[A](x: => A): Future[A] = Future(x)

  override def publicKeys = wrap(keyRing.pub)

  override def toPaillier(in: EncInt): Future[PaillierEnc] = wrap(additive(in))
  override def toElGamal(in: EncInt): Future[ElGamalEnc] =
    wrap(multiplicative(in))
  override def toAes(in: EncInt): Future[AesEnc] = wrap(equality(in))
  override def toOpe(in: EncInt): Future[OpeEnc] = wrap(comparable(in))

  override def convert(s: Scheme)(in: EncInt): Future[EncInt] =
    wrap(Common.convert(keyRing)(s,in))
  override def batchConvert(xs: List[(Scheme,EncInt)]): Future[List[EncInt]] =
    wrap(xs.par.map { case (s,e) => Common.convert(keyRing)(s,e)}.toList)

  override def encrypt(s: Scheme)(in: Int): Future[EncInt] =
    wrap(Common.encrypt(s, keyRing)(in))
  override def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[EncInt]] =
    wrap(xs.par.map { case (s,i) => Common.encrypt(s,keyRing)(i)}.toList)

  override def decryptAndPrint(v: EncInt): Unit = println(Common.decrypt(keyRing.priv)(v))
  override def println[A](a: A): Unit = Predef.println(a)

  override def integerDivide(lhs: EncInt, rhs: EncInt): Future[EncInt] = wrap {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs / plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }

  override def subtract(lhs: EncInt, rhs: EncInt): Future[EncInt] = wrap {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs - plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }

  override def isEven(enc: EncInt): Future[Boolean] = wrap {
    Common.decrypt(keyRing.priv)(enc).mod(2) == 0
  }

  override def isOdd(enc: EncInt): Future[Boolean] = wrap {
    Common.decrypt(keyRing.priv)(enc).mod(2) == 1
  }
}

object CryptoService {
  def start: (ActorSystem, CryptoServicePlus) =
    startWith(4242, KeyRing.create, "cryptoService", 5)
  def startWith(port: Int, keyRing: KeyRing, name: String, numActors: Int):
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
      maximum-frame-size = 512000b
    }
  }
}
""")

    val system = ActorSystem("CryptoService", config)

    // Create numActors routees and a router that mediates
    val cryptoService: CryptoServicePlus = {
      val routees: List[CryptoServicePlus] = List.tabulate(numActors) { i =>
        TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus],
          new CryptoServiceImpl(keyRing)(system.dispatcher)), name + s"_${i}")
      }

      val routeePaths = routees.map { r =>
        TypedActor(system).getActorRefFor(r).path.toStringWithoutAddress
      }

      val router: ActorRef = system.actorOf(
        RoundRobinGroup(routeePaths).props(), name)

      TypedActor(system).typedActorOf(
        TypedProps(classOf[CryptoServicePlus],
          new CryptoServiceImpl(keyRing)(system.dispatcher)), actorRef = router)
    }

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

class DelayedCryptoService(keyRing: KeyRing, d: FiniteDuration)(
  implicit ec: ExecutionContext) extends CryptoServiceImpl(keyRing) {
  override def wrap[A](x: => A): Future[A] = {
    Thread.sleep(d.toMillis)
    Future(x)
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
