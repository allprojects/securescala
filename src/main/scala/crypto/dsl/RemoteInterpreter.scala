package crypto.dsl

import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import crypto._
import crypto.cipher._
import crypto.remote._
import scala.concurrent._
import scala.io.StdIn
import scala.util.Try
  import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._

case class RemoteInterpreter(service: CryptoServicePlus, pubKeys: PubKeys)(
  implicit ctxt: ExecutionContext) extends CryptoInterpreter[Future] {

  override def interpret[A](p: CryptoM[A]): Future[A] = p.resume match {
    case -\/(next) => next match {
      case Coproduct(-\/(cf)) => cf match {
        case Mult(lhs@ElGamalEnc(_,_),rhs@ElGamalEnc(_,_),k) =>
          interpret(k(lhs*rhs))
        case Mult(lhs,rhs,k) => for {
          lhs_ <- service.toElGamal(lhs)
          rhs_ <- service.toElGamal(rhs)
          r <- interpret(k(lhs_ * rhs_))
        } yield r

        case Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k) =>
          interpret(k(lhs+rhs))
        case Plus(lhs,rhs,k) => for {
          lhs_ <- service.toPaillier(lhs)
          rhs_ <- service.toPaillier(rhs)
          r <- interpret(k(lhs_ + rhs_))
        } yield r

        case Compare(lhs@OpeEnc(_),rhs@OpeEnc(_),k) =>
          interpret(k(lhs ?|? rhs))
        case Compare(lhs,rhs,k) => for {
          lhs_ <- service.toOpe(lhs)
          rhs_ <- service.toOpe(rhs)
          r <- interpret(k(lhs_ ?|? rhs_))
        } yield r

        case CompareStr(lhs@OpeString(_),rhs@OpeString(_),k) =>
          interpret(k(lhs ?|? rhs))
        case CompareStr(lhs,rhs,k) => for {
          lhs_ <- service.toOpeStr(lhs)
          rhs_ <- service.toOpeStr(rhs)
          r <- interpret(k(lhs_ ?|? rhs_))
        } yield r

        case ConcatStr(lhs@OpeString(_),rhs@OpeString(_),k) =>
          interpret(k(lhs |+| rhs))
        case ConcatStr(lhs,rhs,k) => for {
          lhs_ <- service.toOpeStr(lhs)
          rhs_ <- service.toOpeStr(rhs)
          r <- interpret(k(lhs_ |+| rhs_))
        } yield r

        case SplitStr(s,r,k) => for {
          s_ <- service.splitStr(s,r)
          r <- interpret(k(IList.fromList(s_)))
        } yield r

        case Equals(lhs@AesEnc(_),rhs@AesEnc(_),k) =>
          interpret(k(lhs === rhs))
        case Equals(lhs,rhs,k) => for {
          lhs_ <- service.toAes(lhs)
          rhs_ <- service.toAes(rhs)
          r <- interpret(k(lhs_ === rhs_))
        } yield r

        case EqualsStr(lhs@AesString(_),rhs@AesString(_),k) =>
          interpret(k(lhs === rhs))
        case EqualsStr(lhs,rhs,k) => for {
          lhs_ <- service.toAesStr(lhs)
          rhs_ <- service.toAesStr(rhs)
          r <- interpret(k(lhs_ === rhs_))
        } yield r

        case ToPaillier(v,k) => v match {
          case v2@PaillierEnc(_) => interpret(k(v2))
          case _ => service.toPaillier(v).flatMap(x => interpret(k(x)))
        }

        case ToGamal(v,k) => v match {
          case v2@ElGamalEnc(_,_) => interpret(k(v2))
          case _ => service.toElGamal(v).flatMap(x => interpret(k(x)))
        }

        case ToAes(v,k) => v match {
          case v2@AesEnc(_) => interpret(k(v2))
          case _ => service.toAes(v).flatMap(x => interpret(k(x)))
        }

        case ToOpe(v,k) => v match {
          case v2@OpeEnc(_) => interpret(k(v2))
          case _ => service.toOpe(v).flatMap(x => interpret(k(x)))
        }

        case ToOpeStr(v,k) => v match {
          case v2@OpeString(_) => interpret(k(v2))
          case _ => service.toOpeStr(v).flatMap(x => interpret(k(x)))
        }

        case ToAesStr(v,k) => v match {
          case v2@AesString(_) => interpret(k(v2))
          case _ => service.toAesStr(v).flatMap(x => interpret(k(x)))
        }

        case Encrypt(s,v,k) =>
          val res: Future[EncInt] = s match {
            // For public key encryption, we do not even have to send anything
            case Additive =>
              Future.successful(Common.depEncryptPub(Additive, pubKeys)(v))
            case Multiplicative =>
              Future.successful(Common.depEncryptPub(Multiplicative, pubKeys)(v))
            case _ => service.encrypt(s)(v)
          }
          res.flatMap(x => interpret(k(x)))

        case Sub(lhs,rhs,k) => service.subtract(lhs,rhs).flatMap(x => interpret(k(x)))
        case Div(lhs,rhs,k) => interpret(k(EncRatio(lhs,rhs)))
        case FloorRatio(r,k) => service.floorRatio(r).flatMap(x => interpret(k(x)))
        case CeilRatio(r,k) => service.ceilRatio(r).flatMap(x => interpret(k(x)))
        case IsEven(v,k) => service.isEven(v).flatMap(x => interpret(k(x)))
        case IsOdd(v,k) => service.isOdd(v).flatMap(x => interpret(k(x)))
      }

      case Coproduct(\/-(em@Embed())) => for {
        v <- interpretA(em.v)
        r <- interpret(em.k(empower(Free.point(v))).join)
      } yield r

    }
    case \/-(x) => Future.successful(x)
  }
}

class RemoteInterpreterOpt(service: CryptoServicePlus, pubKeys: PubKeys)(
  implicit ctxt: ExecutionContext) extends RemoteInterpreter(service, pubKeys)(ctxt) {

  override def interpretA[A](p: Crypto[A]): Future[A] = {
    p.foldMap(new (CryptoF ~> Future) {
      def apply[B](fa: CryptoF[B]): Future[B] = interpret(empower(Free.liftF(fa)))
    })
  }
}

sealed trait BatchOpt
case object SingleBatch extends BatchOpt
case class FixedBatch(size: Int) extends BatchOpt

class RemoteInterpreterOptAnalyze(
  service: CryptoServicePlus,
  pubKeys: PubKeys,
  batchMode: BatchOpt,
  doBatch: Int => Boolean)(
  implicit ctxt: ExecutionContext) extends RemoteInterpreter(service, pubKeys)(ctxt) {

  def parallel[A](p: Crypto[A]): Future[A] = p.foldMap(new (CryptoF ~> Future) {
    def apply[B](fa: CryptoF[B]): Future[B] = interpret(empower(Free.liftF(fa)))
  })

  override def interpretA[A](p: Crypto[A]): Future[A] = {
    val conversions = Analysis.extractConversions(p)
    if (doBatch(conversions.size)) {
      val converted: Future[List[EncInt]] = batchMode match {
        case FixedBatch(batchSize) =>
          Future.traverse(conversions.grouped(batchSize))(service.batchConvert).
            map(_.flatten.toList)
        case SingleBatch =>
          service.batchConvert(conversions)
      }

      converted.map(Analysis.replaceConversions(p).eval(_).run) >>= parallel

    } else {
      parallel(p)
    }
  }
}
