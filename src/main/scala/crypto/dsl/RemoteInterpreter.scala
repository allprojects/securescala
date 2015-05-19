package crypto.dsl

import com.typesafe.config.ConfigFactory

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.util.Try
import scala.io.StdIn

import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._
import scalaz.syntax.order._

import scala.concurrent._
import scala.concurrent.duration._

import crypto._
import crypto.cipher._
import crypto.remote._

case class RemoteInterpreter(service: CryptoServicePlus)(implicit ctxt: ExecutionContext)
    extends CryptoInterpreter[Future] {

  def interpret[A] = _.resume match {

    case -\/(Mult(lhs@ElGamalEnc(_,_),rhs@ElGamalEnc(_,_),k)) => interpret(k(lhs*rhs))
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
      case v2@ElGamalEnc(_,_) => interpret(k(v2))
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

    case -\/(Encrypt(s,v,k)) => service.encrypt(s)(v).flatMap(x => interpret(k(x)))
    case -\/(Sub(lhs,rhs,k)) => service.subtract(lhs,rhs).flatMap(x => interpret(k(x)))
    case -\/(Div(lhs,rhs,k)) =>
      service.integerDivide(lhs,rhs).flatMap(x => interpret(k(x)))

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
      def apply[B](fa: CryptoF[B]): Future[B] = interpret(Free.liftF(fa))
    })
  }
}

object StartCryptoServiceActor extends App {
  val config = ConfigFactory.parseString("""
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = 4242
      maximum-frame-size = 256000b
    }
  }
}
""")

  val keyRing = KeyRing.create

  val system = ActorSystem("CryptoService", config)

  val cryptoService: CryptoServicePlus =
    TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus],
      new CryptoServiceImpl(keyRing)), "cryptoServer")
}

object RunProgramUsingService extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import crypto.dsl.Implicits._

  val service = Await.result(setupCryptoService, 10.seconds)
  val remoteInterpreter = new RemoteInterpreter(service)

  // 2. Get the public keys from the service
  val keys: PubKeys = Await.result(service.publicKeys, 10.seconds)

  // 3. Check for a small program
  val f = remoteInterpreter.interpret(Common.one(keys) + Common.one(keys)).map(service.decryptAndPrint)
  Await.result(f, 60.seconds)

  // 4. Run a program
  while (true) {
    println("factorial of?")
    val input = StdIn.readLine

    if (input.startsWith("q")) System.exit(0)

    val number = Try(input.toInt) match {
      case scala.util.Success(x) => \/-(x)
      case scala.util.Failure(e) => -\/(e)
    }
    number match {
      case \/-(i) =>
        val result = remoteInterpreter.interpret {
          ExamplePrograms.factorial(Common.encryptPub(Additive, keys)(i).valueOr(sys.error))
        }
        val finalRes = Await.result(result, 60.minutes)
        service.decryptAndPrint(finalRes)
      case -\/(e) => println("I didn't understand that")
    }
  }


  /////////// setup

  private def setupCryptoService: Future[CryptoServicePlus] = {

    val config = ConfigFactory.parseString("""
akka {
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

    implicit val timeOut = Timeout(10.seconds)
    val futureRef =
      system.actorSelection("akka.tcp://CryptoService@127.0.0.1:4242/user/cryptoServer").
        resolveOne().map { ref =>
          TypedActor(system).typedActorOf(TypedProps(classOf[CryptoServicePlus]).
            withTimeout(timeOut), ref)
        }
    futureRef
  }
}
