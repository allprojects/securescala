package crypto.remote

import akka.actor._
import akka.pattern.ask

import scala.concurrent._
import scala.concurrent.duration._

import crypto._
import crypto.cipher._

trait CryptoService {
  def toPaillier(in: Enc): Future[PaillierEnc]
  def toElGamal(in: Enc): Future[GamalEnc]
  def toAes(in: Enc): Future[AesEnc]
  def toOpe(in: Enc): Future[OpeEnc]
  def convert(s: Scheme, in: Enc): Future[Enc]
}

class CryptoServiceImpl(keyRing: KeyRing) extends CryptoService {
  private val doConvert = Common.convert(keyRing)

  override def toPaillier(in: Enc): Future[PaillierEnc] = Future.successful {
    val r@PaillierEnc(_) = doConvert(Additive, in)
    r
  }

  override def toElGamal(in: Enc): Future[GamalEnc] = Future.successful {
    val r@GamalEnc(_,_) = doConvert(Multiplicative, in)
    r
  }

  override def toAes(in: Enc): Future[AesEnc] = Future.successful {
    val r@AesEnc(_) = doConvert(Equality, in)
    r
  }

  override def toOpe(in: Enc): Future[OpeEnc] = Future.successful {
    val r@OpeEnc(_) = doConvert(Comparable, in)
    r
  }

  override def convert(s: Scheme, in: Enc): Future[Enc] = Future.successful {
    Common.convert(keyRing)(s,in)
  }
}

object CryptoServiceActor extends App {

  val keyRing = KeyRing.create

  val system = ActorSystem("CryptoService")

  val cryptoService: CryptoService =
    TypedActor(system).typedActorOf(TypedProps(classOf[CryptoService],
      new CryptoServiceImpl(keyRing)), "cryptoServer")

  val response: Future[Enc] = cryptoService.toPaillier(PaillierEnc(1))
  println { Await.result(response, 1.second) }

  TypedActor(system).poisonPill(cryptoService)

  system.shutdown()
}
