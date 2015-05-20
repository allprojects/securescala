package crypto.dsl

import com.typesafe.config.ConfigFactory

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

object FactaaS extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import ExamplePrograms.factorial

  print("Trying to connect to crypto service...")
  val service = Await.result(CryptoService.connect, 10.seconds)
  println("ok")

  val remoteInterpreter = new RemoteInterpreter(service)

  print("Requesting public keys...")
  val keys: PubKeys = Await.result(service.publicKeys, 10.seconds)
  println("ok")

  def loop(): Unit = {
    println("Your input:")
    val input = StdIn.readLine
    if (input.startsWith("quit")) {

    } else {
      \/.fromTryCatchNonFatal(input.toInt) match {
        case \/-(i) =>
          Common.encryptPub(Additive, keys)(i) match {
            case -\/(err) => println("Failed during public key encryption with: " + err)
            case \/-(encryptedInput) =>
              val result = remoteInterpreter.interpret {
                factorial(encryptedInput)
              }
              val finalRes = Await.result(result, 60.minutes)
              service.decryptAndPrint(finalRes)
          }
        case -\/(e) => "Invalid input, type `quit` to exit"
      }
      loop
    }
  }

  println("Entering input loop.")
  loop()
}
