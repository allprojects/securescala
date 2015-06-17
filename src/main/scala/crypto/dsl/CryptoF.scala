package crypto.dsl

import scalaz._

import crypto._
import crypto.cipher._

sealed trait CryptoF[+K]
sealed trait CryptoString[+K] extends CryptoF[K]
case class CompareStr[K](lhs: EncString, rhs: EncString,k: Ordering => K) extends CryptoString[K]
case class EqualsStr[K](lhs: EncString, rhs: EncString, k: Boolean => K) extends CryptoString[K]
case class ToAesStr[K](v: EncString, k: AesString => K) extends CryptoString[K]
case class ToOpeStr[K](v: EncString, k: OpeString => K) extends CryptoString[K]
case class ConcatStr[K](s1: EncString, s2: EncString, k: EncString => K)
    extends CryptoString[K]
case class SplitStr[K](s: EncString, regex: String, k: List[EncString] => K)
    extends CryptoString[K]

sealed trait CryptoNumber[+K] extends CryptoF[K]
case class Mult[K](lhs: EncInt, rhs: EncInt, k: ElGamalEnc => K) extends CryptoNumber[K]
case class Plus[K](lhs: EncInt, rhs: EncInt, k: PaillierEnc => K) extends CryptoNumber[K]
case class Equals[K](lhs: EncInt, rhs: EncInt, k: Boolean => K) extends CryptoNumber[K]
case class Compare[K](lhs: EncInt, rhs: EncInt, k: Ordering => K) extends CryptoNumber[K]

case class Encrypt[K](s:Scheme, v: Int, k: EncInt => K) extends CryptoNumber[K]
case class ToPaillier[K](v: EncInt, k: PaillierEnc => K) extends CryptoNumber[K]
case class ToGamal[K](v: EncInt, k: ElGamalEnc => K) extends CryptoNumber[K]
case class ToAes[K](v: EncInt, k: AesEnc => K) extends CryptoNumber[K]
case class ToOpe[K](v: EncInt, k: OpeEnc => K) extends CryptoNumber[K]

case class Sub[K](lhs: EncInt, rhs: EncInt, k: EncInt => K) extends CryptoNumber[K]
case class Div[K](lhs: EncInt, rhs: EncInt, k: EncInt => K) extends CryptoNumber[K]
case class IsEven[K](v: EncInt, k: Boolean => K) extends CryptoNumber[K]
case class IsOdd[K](v: EncInt, k: Boolean => K) extends CryptoNumber[K]

object CryptoString {
  implicit val instance: Functor[CryptoString] = new Functor[CryptoString] {
    def map[A,B](fa: CryptoString[A])(f: A => B): CryptoString[B] = fa match {
      case EqualsStr(lhs,rhs,k) => EqualsStr(lhs,rhs,f compose k)
      case CompareStr(lhs,rhs,k) => CompareStr(lhs,rhs,f compose k)
      case ToAesStr(v,k) => ToAesStr(v,f compose k)
      case ToOpeStr(v,k) => ToOpeStr(v,f compose k)
      case ConcatStr(s1,s2,k) => ConcatStr(s1,s2,f compose k)
      case SplitStr(s,r,k) => SplitStr(s,r,f compose k)
    }
  }
}

object CryptoNumber {
  implicit val instance: Functor[CryptoNumber] = new Functor[CryptoNumber] {
    def map[A,B](fa: CryptoNumber[A])(f: A => B): CryptoNumber[B] = fa match {
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
    }
  }
}

object CryptoF {
  implicit val instance: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case fb: CryptoNumber[A] => Functor[CryptoNumber].map(fb)(f)
      case fb: CryptoString[A] => Functor[CryptoString].map(fb)(f)
    }
  }
}

// Use type member to encode the existential type
// data Embed k = Embed (forall a. (Crypto a) (CryptoM a -> CryptoM k))
abstract case class Embed[K]() {
  type I
  val v: Crypto[I]
  val k: CryptoM[I] => CryptoM[K]
}

object Embed {
  implicit val embedFunctor: Functor[Embed] = new Functor[Embed] {
    def map[A,B](fa: Embed[A])(f: A => B): Embed[B] = fa match {
      case e@Embed() => new Embed[B] {
        type I = e.I
        val v: Crypto[I] = e.v
        val k = (x: CryptoM[I]) => e.k(x).map(f)
      }
    }
  }
}
