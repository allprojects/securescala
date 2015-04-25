package crypto.dsl

import crypto.cipher._
import scalaz._

sealed trait CryptoF[+K]
case class Mult[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]
case class Plus[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]
case class Encrypt[K](v: Int, k: Enc => K) extends CryptoF[K]
case class Equals[K](lhs: Enc, rhs: Enc, k: Boolean => K)  extends CryptoF[K]
case class Compare[K](lhs: Enc, rhs: Enc, k: Ordering => K) extends CryptoF[K]

object CryptoF {
  object DSL {
    type CryptoM[A] = Free[CryptoF, A]
    type Crypto[A] = FreeAp[CryptoF, A]

    def multiply(lhs: Enc, rhs: Enc): CryptoM[Enc] = Free.liftF(Mult(lhs,rhs,identity))
    def add(lhs: Enc, rhs: Enc): CryptoM[Enc] = Free.liftF(Plus(lhs,rhs,identity))
    def encrypt(v: Int): CryptoM[Enc] = Free.liftF(Encrypt(v,identity))
    def equals(lhs: Enc, rhs: Enc): CryptoM[Boolean] = Free.liftF(Equals(lhs,rhs,identity))
    def compare(lhs: Enc, rhs: Enc): CryptoM[Ordering] = Free.liftF(Compare(lhs,rhs,identity))
  }

  // deriving Functor
  implicit val functor: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case Mult(lhs,rhs,k) => Mult(lhs,rhs,f compose k)
      case Plus(lhs,rhs,k) => Plus(lhs,rhs,f compose k)
      case Encrypt(v,k) => Encrypt(v,f compose k)
      case Equals(lhs,rhs,k) => Equals(lhs,rhs,f compose k)
      case Compare(lhs,rhs,k) => Compare(lhs,rhs,f compose k)
    }
  }
}
