package crypto.dsl

import akka.actor._
import akka.pattern.ask

import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._
import scalaz.syntax.order._
import scalaz.syntax.traverse._

import scala.concurrent._
import scala.concurrent.duration._

import crypto._
import crypto.remote._

case class RemoteInterpreter(service: CryptoServicePlus)(implicit ctxt: ExecutionContext)
    extends CryptoInterpreter[Future] {

  def interpret[A] = _.resume match {

    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) => for {
      lhs_ <- service.toElGamal(lhs)
      rhs_ <- service.toElGamal(rhs)
      r <- interpret(k(lhs_ * rhs_))
    } yield r

    case -\/(Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k)) => interpret(k(lhs+rhs))
    case -\/(Plus(lhs,rhs,k)) => for {
      lhs_ <- service.toPaillier(lhs)
      rhs_ <- service.toPaillier(rhs)
      r <- interpret(k(lhs_ + rhs_))
    } yield r

    case -\/(Compare(lhs@OpeEnc(_),rhs@OpeEnc(_),k)) => interpret(k(lhs ?|? rhs))
    case -\/(Compare(lhs,rhs,k)) => for {
      lhs_ <- service.toOpe(lhs)
      rhs_ <- service.toOpe(rhs)
      r <- interpret(k(lhs_ ?|? rhs_))
    } yield r

    case -\/(Equals(lhs@AesEnc(_),rhs@AesEnc(_),k)) => interpret(k(lhs =:= rhs))
    case -\/(Equals(lhs,rhs,k)) => for {
      lhs_ <- service.toAes(lhs)
      rhs_ <- service.toAes(rhs)
      r <- interpret(k(lhs_ =:= rhs_))
    } yield r

    case -\/(ToPaillier(v,k)) => v match {
      case v2@PaillierEnc(_) => interpret(k(v2))
      case _ => service.toPaillier(v).flatMap(x => interpret(k(x)))
    }

    case -\/(ToGamal(v,k)) => v match {
      case v2@GamalEnc(_,_) => interpret(k(v2))
      case _ => service.toElGamal(v).flatMap(x => interpret(k(x)))
    }

    case -\/(ToAes(v,k)) => v match {
      case v2@AesEnc(_) => interpret(k(v2))
      case _ => service.toAes(v).flatMap(x => interpret(k(x)))
    }

    case -\/(ToOpe(v,k)) => v match {
      case v2@OpeEnc(_) => interpret(k(v2))
      case _ => service.toOpe(v).flatMap(x => interpret(k(x)))
    }

    case -\/(Encrypt(s,v,k)) => sys.error("encryption")

    case -\/(Sub(lhs,rhs,k)) => sys.error("subtraction")

    case -\/(Div(lhs,rhs,k)) => sys.error("division")

    case -\/(Embed(p,k)) => for {
      v <- interpretA(p)
      r <- interpret(k(Free.point(v)).join)
    } yield r

    case \/-(x) =>
      Future.successful(x)
  }

  def interpretA[A](p: Crypto[A]): Future[A] = {
    p.foldMap(new (CryptoF ~> Future) {
      // Peform regular interpretation inside future
      def apply[A](fa: CryptoF[A]): Future[A] = interpret(Free.liftF(fa))
    })
  }
}

object ActorInterpretation extends App {
  val keyRing = KeyRing.create

  val system = ActorSystem("CryptoService")

  val cryptoService = new CryptoServiceImpl(keyRing)

  // val cryptoService: CryptoServicePlus =
  //   TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus],
  //     new CryptoServiceImpl(keyRing)), "cryptoServer")

  import scala.concurrent.ExecutionContext.Implicits.global
  val remoteInterpreter = new RemoteInterpreter(cryptoService)

  ///////////////////////////////////////////////////////////////////////

  import crypto.cipher._
  import scalaz.std.list._
  val \/-(encryptedList) = SampleData.fixed1.map(Common.encryptPub(Multiplicative, keyRing.pub)).sequenceU

  val \/-(zero@PaillierEnc(_)) = Common.encryptPub(Additive, keyRing.pub)(0)

  val result = remoteInterpreter.interpret {
    sumA(zero)(encryptedList)
  }

  val r = Common.decrypt(keyRing.priv)(Await.result(result, Duration.Inf))

  println(r)

  ///////////////////////////////////////////////////////////////////////

  TypedActor(system).poisonPill(cryptoService)

  system.shutdown()
}
