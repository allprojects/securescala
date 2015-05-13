package crypto

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.std.list._
import scalaz.syntax.traverse._

import crypto.cipher._

package object dsl extends BaseDsl with DeriveDsl {
  object base extends BaseDsl
  object Implicits {
    implicit def liftCryptoToMonadic[A](p: Crypto[A]): CryptoM[A] = embed(p)
  }
}

trait BaseDsl {
  import crypto.dsl._

  type Crypto[A] = FreeAp[CryptoF, A]
  type CryptoM[A] = Free[CryptoF, A]

  def multiply(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Mult(lhs,rhs,identity))
  def add(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Plus(lhs,rhs,identity))
  def equal(lhs: Enc, rhs: Enc): Crypto[Boolean] = FreeAp.lift(Equals(lhs,rhs,identity))
  def compare(lhs: Enc, rhs: Enc): Crypto[Ordering] = FreeAp.lift(Compare(lhs,rhs,identity))

  def encrypt(s: Scheme)(v: Int): Crypto[Enc] = FreeAp.lift(Encrypt(s,v,identity))
  def toPaillier(v: Enc): Crypto[PaillierEnc] = FreeAp.lift(ToPaillier(v,identity))
  def toGamal(v: Enc): Crypto[GamalEnc] = FreeAp.lift(ToGamal(v,identity))
  def toAes(v: Enc): Crypto[AesEnc] = FreeAp.lift(ToAes(v,identity))
  def toOpe(v: Enc): Crypto[OpeEnc] = FreeAp.lift(ToOpe(v,identity))

  def subtract(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Sub(lhs,rhs,identity))
  def divide(lhs: Enc, rhs: Enc): Crypto[Enc] = FreeAp.lift(Div(lhs,rhs,identity))

  def embed[A](v: Crypto[A]): CryptoM[A] = Free.liftF(Embed(v,(x: CryptoM[A]) => x))

}

trait DeriveDsl {
  self: BaseDsl =>
  import dsl.Implicits._

  def sumM[F[_]:Foldable](zero: PaillierEnc)(xs: F[Enc]): CryptoM[Enc] =
    xs.foldLeftM[CryptoM,Enc](zero)(add(_,_))

  def sumA[F[_]:Traverse](zero: PaillierEnc)(xs: F[Enc]): Crypto[PaillierEnc] =
    xs.traverse(toPaillier(_)).map(_.foldLeft(zero)(_+_))

  def productM[F[_]:Foldable](one: GamalEnc)(xs: F[Enc]): CryptoM[Enc] =
    xs.foldLeftM[CryptoM,Enc](one)(multiply(_,_))

  def productA[F[_]:Traverse](one: GamalEnc)(xs: F[Enc]): Crypto[GamalEnc] =
    xs.traverse(toGamal(_)).map(_.foldLeft(one)(_*_))

  def average[F[_]:Traverse](zero: PaillierEnc)(xs: F[Enc]): CryptoM[Enc] = for {
    sum <- sumA(zero)(xs)
    n <- encrypt(Additive) { xs.length }
    r <- divide(sum,n)
  } yield r

  def sorted(xs: List[Enc]): Crypto[List[OpeEnc]] =
    xs.traverse(toOpe).map(_.sorted)
}
