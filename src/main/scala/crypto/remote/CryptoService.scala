package crypto.remote

import akka.actor._
import akka.pattern.ask

import scala.concurrent._
import scala.concurrent.duration._

import crypto._

trait CryptoService {
  def toPaillier: Enc => Future[PaillierEnc]
}

class CryptoServiceImpl(keyRing: KeyRing) extends CryptoService {
  val KeyRing(pub,priv) = keyRing

  override def toPaillier: Enc => Future[PaillierEnc] = _ match {
    case x@PaillierEnc(_) => Future.successful(x)
    case x => sys.error(s"Don't know how to convert to paillier: ${x}")
  }
}

object CryptoServiceActor extends App {

  val system = ActorSystem("CryptoService")

  val cryptoService: CryptoService =
    TypedActor(system).typedActorOf(TypedProps[CryptoServiceImpl]())

  val response: Future[Enc] = cryptoService.toPaillier(PaillierEnc(1))
  println { Await.result(response, 1.second) }

  TypedActor(system).poisonPill(cryptoService)

  system.shutdown()
}
