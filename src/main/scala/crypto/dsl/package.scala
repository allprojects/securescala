package crypto

import scala.language.higherKinds

import scalaz._
import Scalaz._

import crypto.cipher._

package object dsl {
  type Crypto[A] = FreeAp[CryptoF, A]
  type CryptoM[A] = Free[CryptoF, A]

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

  // Convenience

  def sumM[F[_]:Foldable](zero: PaillierEnc)(xs: List[Enc]): CryptoM[Enc] =
    xs.foldLeftM[CryptoM,Enc](zero)(add(_,_).monadic)

  def sumA[F[_]:Traverse](zero: PaillierEnc)(xs: F[Enc]): Crypto[Enc] =
    xs.traverse(toPaillier(_)).map(_.foldLeft(zero)(_+_))
}
