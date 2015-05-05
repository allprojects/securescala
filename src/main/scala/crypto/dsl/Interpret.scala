package crypto.dsl

import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._

import scala.concurrent._

import crypto.cipher._
import crypto._

trait CryptoInterpreter {
  def interpret[A]: CryptoM[A] => A
  def interpretA[A]: Crypto[A] => A
}

case class LocalInterpreter(keyRing: KeyRing) extends CryptoInterpreter {
  val KeyRing(pub,priv) = keyRing

  def interpPar[A](p: Crypto[A])(
    implicit executionContext: ExecutionContext): Future[A] = {

    p.foldMap(new (CryptoF ~> Future) {
      def apply[A](fa: CryptoF[A]): Future[A] = Future {
        interpret(Free.liftF(fa))
      }
    })
  }

  // TODO take advantage of applicative structure to do optimizations
  def interpretA[A]: Crypto[A] => A = x => {
    interpret(x.monadic)
  }

  def interpret[A]: CryptoM[A] => A = _.resume match {
    // Multiplication
    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) =>
      val lhs2@GamalEnc(_,_) = Common.convert(pub, priv)(Multiplicative, lhs)
      val rhs2@GamalEnc(_,_) = Common.convert(pub, priv)(Multiplicative, rhs)
      interpret(k(lhs2*rhs2))

    // Addition
    case -\/(Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k)) => interpret(k(lhs+rhs))
    case -\/(Plus(lhs,rhs,k)) =>
      val PaillierEnc(lhs_) = Common.convert(pub, priv)(Additive, lhs)
      val PaillierEnc(rhs_) = Common.convert(pub, priv)(Additive, rhs)
      val r = PaillierEnc((lhs_ * rhs_) mod pub.paillier.nSquare)
      interpret(k(r))

    // Comparisons
    case -\/(Compare(lhs,rhs,k)) => sys.error("No scheme for comparison")

    // Equality
    case -\/(Equals(lhs@AesEnc(_),rhs@AesEnc(_),k)) => interpret(k(lhs =:= rhs))
    case -\/(Equals(lhs,rhs,k)) =>
      val decLhs = Common.decrypt(priv)(lhs).toByteArray
      val decRhs = Common.decrypt(priv)(rhs).toByteArray

      val encLhs: AesEnc = AesEnc(priv.aesEnc(decLhs))
      val encRhs: AesEnc = AesEnc(priv.aesEnc(decRhs))
      interpret(k(encLhs =:= encRhs))

    // Encryption
    case -\/(Encrypt(v,k)) =>
      // TODO be more clever about what scheme to use
      interpret(k(Common.encrypt(Additive, pub)(v)))

    case -\/(ToPaillier(v,k)) =>
      val r@PaillierEnc(_) = Common.convert(pub,priv)(Additive,v)
      interpret(k(r))

    case -\/(ToGamal(v,k)) =>
      val r@GamalEnc(_,_) = Common.convert(pub,priv)(Multiplicative,v)
      interpret(k(r))

    case -\/(ToAes(v,k)) =>
      val r = Common.decrypt(priv)(v)
      interpret(k(AesEnc(priv.aesEnc(r.toByteArray))))

      // Offline operations?
    case -\/(Sub(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(priv)(lhs)
      val plainRhs = Common.decrypt(priv)(rhs)
      val r = Common.encrypt(Additive, pub)(plainLhs - plainRhs)
      interpret(k(r))

    case -\/(Div(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(priv)(lhs)
      val plainRhs = Common.decrypt(priv)(rhs)
      val r = Common.encrypt(Additive, pub)(plainLhs / plainRhs)
      interpret(k(r))

    case -\/(Embed(p,k)) =>
      val r: CryptoM[A] = k(Free.point(interpretA(p))).join
      interpret(r)

    // End of the program, return the final value
    case \/-(x) => x
  }
}
