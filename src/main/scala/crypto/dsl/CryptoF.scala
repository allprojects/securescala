package crypto.dsl

import crypto.cipher._
import scalaz._

sealed trait CryptoF[+K]
case class Mult[K](lhs: Enc, rhs: Enc, k: GamalEnc => K) extends CryptoF[K]
case class Plus[K](lhs: Enc, rhs: Enc, k: PaillierEnc => K) extends CryptoF[K]
case class Equals[K](lhs: Enc, rhs: Enc, k: Boolean => K)  extends CryptoF[K]
case class Compare[K](lhs: Enc, rhs: Enc, k: Ordering => K) extends CryptoF[K]

case class Encrypt[K](v: Int, k: Enc => K) extends CryptoF[K]
case class ToPaillier[K](v: Enc, k: PaillierEnc => K) extends CryptoF[K]
case class ToGamal[K](v: Enc, k: GamalEnc => K) extends CryptoF[K]
case class ToAes[K](v: Enc, k: AesEnc => K) extends CryptoF[K]

// TODO offline or encode into encryption?
case class Sub[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]
case class Div[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]

object CryptoF {
  object DSL {
    type CryptoM[A] = Free[CryptoF, A]
    type Crypto[A] = FreeAp[CryptoF, A]

    def multiply(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Mult(lhs,rhs,identity))
    def add(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Plus(lhs,rhs,identity))
    def equal(lhs: Enc, rhs: Enc): Crypto[Boolean] = FreeAp.lift(Equals(lhs,rhs,identity))
    def compare(lhs: Enc, rhs: Enc): Crypto[Ordering] = FreeAp.lift(Compare(lhs,rhs,identity))
    def encrypt(v: Int): Crypto[Enc] = FreeAp.lift(Encrypt(v,identity))
    def toPaillier(v: Enc): Crypto[PaillierEnc] = FreeAp.lift(ToPaillier(v,identity))
    def toGamal(v: Enc): Crypto[GamalEnc] = FreeAp.lift(ToGamal(v,identity))
    def toAes(v: Enc): Crypto[AesEnc] = FreeAp.lift(ToAes(v,identity))

    def subtract(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Sub(lhs,rhs,identity))
    def divide(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Div(lhs,rhs,identity))
  }

  // deriving Functor
  implicit val functor: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case Mult(lhs,rhs,k) => Mult(lhs,rhs,f compose k)
      case Plus(lhs,rhs,k) => Plus(lhs,rhs,f compose k)
      case Equals(lhs,rhs,k) => Equals(lhs,rhs,f compose k)
      case Compare(lhs,rhs,k) => Compare(lhs,rhs,f compose k)
      case Encrypt(v,k) => Encrypt(v,f compose k)
      case ToPaillier(v,k) => ToPaillier(v,f compose k)
      case ToGamal(v,k) => ToGamal(v,f compose k)
      case ToAes(v,k) => ToAes(v,f compose k)
      case Sub(lhs,rhs,k) => Sub(lhs,rhs,f compose k)
      case Div(lhs,rhs,k) => Div(lhs,rhs,f compose k)
    }
  }
}
