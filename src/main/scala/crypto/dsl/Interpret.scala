package crypto.dsl

import scala.language.higherKinds

import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._

import scala.concurrent._

import crypto.cipher._
import crypto._

/**
  * An interpreter for a program written using the crypto dsl.  It
  * offers different implementations of programs depending on the
  * style it is written in (applicative vs monadic)
  */

trait CryptoInterpreter[F[_]] {
  /**
    * Interpret a program written in the monadic DSL and return the result
    */
  def interpret[A]: CryptoM[A] => F[A]

  /**
    * Interpret a program written in the applicative DSL and therefore
    * does not depend on the order of the effects.
    *
    * By default this performs no optimizations, you need to override
    * it and take advantage of the applicative structure
    */
  def interpretA[A]: Crypto[A] => F[A] = x => interpret(x.monadic)
}

// Why λ[α=>α]?
// - This is equivalent to `type Identity[A] = A` enabling the
//   interpreter to return a type that is not higher kinded
case class LocalInterpreter(keyRing: KeyRing) extends CryptoInterpreter[λ[α=>α]] {
  val KeyRing(pub,priv) = keyRing

  // One possible optimization is to use futures
  def interpPar[A](p: Crypto[A])(
    implicit executionContext: ExecutionContext): Future[A] = {

    p.foldMap(new (CryptoF ~> Future) {
      // Peform regular interpretation inside future
      def apply[A](fa: CryptoF[A]): Future[A] = Future { interpret(Free.liftF(fa)) }
    })
  }

  def interpret[A]: CryptoM[A] => A = _.resume match {
    // Multiplication
    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) =>
      val lhs2@GamalEnc(_,_) = Common.convert(keyRing)(Multiplicative, lhs)
      val rhs2@GamalEnc(_,_) = Common.convert(keyRing)(Multiplicative, rhs)
      interpret(k(lhs2*rhs2))

    // Addition
    case -\/(Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k)) => interpret(k(lhs+rhs))
    case -\/(Plus(lhs,rhs,k)) =>
      val PaillierEnc(lhs_) = Common.convert(keyRing)(Additive, lhs)
      val PaillierEnc(rhs_) = Common.convert(keyRing)(Additive, rhs)
      val r = PaillierEnc((lhs_ * rhs_) mod pub.paillier.nSquare)
      interpret(k(r))

    // Comparisons
    case -\/(Compare(lhs,rhs,k)) => sys.error("No scheme for comparison")

    // Equality
    case -\/(Equals(lhs@AesEnc(_),rhs@AesEnc(_),k)) => interpret(k(lhs =:= rhs))
    case -\/(Equals(lhs,rhs,k)) =>
      val lhs2@AesEnc(_) = Common.convert(keyRing)(Equality,lhs)
      val rhs2@AesEnc(_) = Common.convert(keyRing)(Equality,rhs)
      interpret(k(lhs2 =:= rhs2))

    // Encryption
    case -\/(Encrypt(v,k)) =>
      // TODO be more clever about what scheme to use
      interpret(k(Common.encryptPub(Additive, pub)(v)))

    case -\/(ToPaillier(v,k)) =>
      val r@PaillierEnc(_) = Common.convert(keyRing)(Additive,v)
      interpret(k(r))

    case -\/(ToGamal(v,k)) =>
      val r@GamalEnc(_,_) = Common.convert(keyRing)(Multiplicative,v)
      interpret(k(r))

    case -\/(ToAes(v,k)) =>
      val r@AesEnc(_) = Common.convert(keyRing)(Equality,v)
      interpret(k(r))

    case -\/(ToOpe(v,k)) =>
      val r@OpeEnc(_) = Common.convert(keyRing)(Comparable,v)
      interpret(k(r))

      // Offline operations?
    case -\/(Sub(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(priv)(lhs)
      val plainRhs = Common.decrypt(priv)(rhs)
      val r = Common.encryptPub(Additive, pub)(plainLhs - plainRhs)
      interpret(k(r))

    case -\/(Div(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(priv)(lhs)
      val plainRhs = Common.decrypt(priv)(rhs)
      val r = Common.encryptPub(Additive, pub)(plainLhs / plainRhs)
      interpret(k(r))

    case -\/(Embed(p,k)) =>
      val r: CryptoM[A] = k(Free.point(interpretA(p))).join
      interpret(r)

    // End of the program, return the final value
    case \/-(x) => x
  }
}
