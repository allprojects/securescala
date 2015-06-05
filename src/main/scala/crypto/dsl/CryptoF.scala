package crypto.dsl

import scalaz._

import crypto._
import crypto.cipher._

sealed trait CryptoF[+K]
case class Mult[K](lhs: EncInt, rhs: EncInt, k: ElGamalEnc => K) extends CryptoF[K]
case class Plus[K](lhs: EncInt, rhs: EncInt, k: PaillierEnc => K) extends CryptoF[K]
case class Equals[K](lhs: EncInt, rhs: EncInt, k: Boolean => K)  extends CryptoF[K]
case class Compare[K](lhs: EncInt, rhs: EncInt, k: Ordering => K) extends CryptoF[K]

case class Encrypt[K](s:Scheme, v: Int, k: EncInt => K) extends CryptoF[K]
case class ToPaillier[K](v: EncInt, k: PaillierEnc => K) extends CryptoF[K]
case class ToGamal[K](v: EncInt, k: ElGamalEnc => K) extends CryptoF[K]
case class ToAes[K](v: EncInt, k: AesEnc => K) extends CryptoF[K]
case class ToOpe[K](v: EncInt, k: OpeEnc => K) extends CryptoF[K]

case class Sub[K](lhs: EncInt, rhs: EncInt, k: EncInt => K) extends CryptoF[K]
case class Div[K](lhs: EncInt, rhs: EncInt, k: EncInt => K) extends CryptoF[K]
case class IsEven[K](v: EncInt, k: Boolean => K) extends CryptoF[K]
case class IsOdd[K](v: EncInt, k: Boolean => K) extends CryptoF[K]

// Embed the applicative part into monadic language
case class Embed[A,K](v: Crypto[A], k: CryptoM[A] => CryptoM[K]) extends CryptoF[K]

object CryptoF {
  // deriving Functor
  implicit val functor: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case Mult(lhs,rhs,k) => Mult(lhs,rhs,f compose k)
      case Plus(lhs,rhs,k) => Plus(lhs,rhs,f compose k)
      case Equals(lhs,rhs,k) => Equals(lhs,rhs,f compose k)
      case Compare(lhs,rhs,k) => Compare(lhs,rhs,f compose k)
      case Encrypt(s,v,k) => Encrypt(s,v,f compose k)
      case ToPaillier(v,k) => ToPaillier(v,f compose k)
      case ToGamal(v,k) => ToGamal(v,f compose k)
      case ToAes(v,k) => ToAes(v,f compose k)
      case ToOpe(v,k) => ToOpe(v,f compose k)
      case Sub(lhs,rhs,k) => Sub(lhs,rhs,f compose k)
      case Div(lhs,rhs,k) => Div(lhs,rhs,f compose k)
      case IsEven(v,k) => IsEven(v,f compose k)
      case IsOdd(v,k) => IsOdd(v,f compose k)
      case e: Embed[a,k] => Embed(e.v,(x: CryptoM[a]) => e.k(x).map(f))
    }
  }
}
