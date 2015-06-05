package crypto

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._

import scalaz.std.boolean.{fold => cond}
import scalaz.Leibniz._
import scalaz.Ordering._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.semigroup._

import crypto.cipher._

package object dsl extends BaseDsl with DeriveDsl {
  object base extends BaseDsl
  object Implicits {
    implicit def liftCryptoToMonadic[A](p: Crypto[A]): CryptoM[A] = embed(p)
    implicit class EncInfixOps(self: EncInt) {
      def *(that: EncInt) = multiply(self,that)
      def +(that: EncInt) = add(self,that)
      def -(that: EncInt) = subtract(self,that)
      def /(that: EncInt) = divide(self,that)
      def =:=(that: EncInt) = equal(self,that)
      def ?|?(that: EncInt) = compare(self,that)
      def <(that: EncInt) = compare(self,that).map(_ == LT)
      def <=(that: EncInt) = compare(self,that).map(x => x == LT || x == EQ)
      def >(that: EncInt) = compare(self,that).map(_ == GT)
      def >=(that: EncInt) = compare(self,that).map(x => x == GT || x == EQ)
    }
  }
}

trait BaseDsl {
  import crypto.dsl._

  type Crypto[A] = FreeAp[CryptoF, A]
  type CryptoM[A] = Free[CryptoF, A]

  def multiply(lhs: EncInt, rhs: EncInt): Crypto[EncInt] = FreeAp.lift(Mult(lhs,rhs,identity))
  def add(lhs: EncInt, rhs: EncInt): Crypto[EncInt] = FreeAp.lift(Plus(lhs,rhs,identity))
  def equal(lhs: EncInt, rhs: EncInt): Crypto[Boolean] = FreeAp.lift(Equals(lhs,rhs,identity))
  def compare(lhs: EncInt, rhs: EncInt): Crypto[Ordering] = FreeAp.lift(Compare(lhs,rhs,identity))

  def encrypt(s: Scheme)(v: Int): Crypto[EncInt] = FreeAp.lift(Encrypt(s,v,identity))
  def toPaillier(v: EncInt): Crypto[PaillierEnc] = FreeAp.lift(ToPaillier(v,identity))
  def toGamal(v: EncInt): Crypto[ElGamalEnc] = FreeAp.lift(ToGamal(v,identity))
  def toAes(v: EncInt): Crypto[AesEnc] = FreeAp.lift(ToAes(v,identity))
  def toOpe(v: EncInt): Crypto[OpeEnc] = FreeAp.lift(ToOpe(v,identity))

  def subtract(lhs: EncInt, rhs: EncInt): Crypto[EncInt] = FreeAp.lift(Sub(lhs,rhs,identity))
  def divide(lhs: EncInt, rhs: EncInt): Crypto[EncInt] = FreeAp.lift(Div(lhs,rhs,identity))

  def isEven(v: EncInt): Crypto[Boolean] = FreeAp.lift(IsEven(v,identity))
  def isOdd(v: EncInt): Crypto[Boolean] = FreeAp.lift(IsOdd(v,identity))

  def embed[A](v: Crypto[A]): CryptoM[A] = Free.liftF(Embed(v,(x: CryptoM[A]) => x))
}

trait DeriveDsl {
  deriveDsl: BaseDsl =>
  import dsl.Implicits._

  def sumM[F[_]:Foldable](zero: PaillierEnc)(xs: F[EncInt]): CryptoM[EncInt] =
    xs.foldLeftM[CryptoM,EncInt](zero)(add(_,_))

  def sumA[F[_]:Traverse](zero: PaillierEnc)(xs: F[EncInt]): Crypto[PaillierEnc] =
    sumOpt(xs).map(_.getOrElse(zero))

  def sumOpt[F[_]:Traverse](xs: F[EncInt]): Crypto[Option[PaillierEnc]] =
    xs.traverse(toPaillier).map(_.foldLeft(None: Option[PaillierEnc])(_ ⊹ Some(_)))

  def productM[F[_]:Foldable](one: ElGamalEnc)(xs: F[EncInt]): CryptoM[EncInt] =
    xs.foldLeftM[CryptoM,EncInt](one)(multiply(_,_))

  def productA[F[_]:Traverse](one: ElGamalEnc)(xs: F[EncInt]): Crypto[ElGamalEnc] =
    productOpt(xs).map(_.getOrElse(one))

  def productOpt[F[_]:Traverse](xs: F[EncInt]): Crypto[Option[ElGamalEnc]] =
    xs.traverse(toGamal).map(_.foldLeft(None: Option[ElGamalEnc])(_ ⊹ Some(_)))

  def average[F[_]:Traverse](zero: PaillierEnc)(xs: F[EncInt]): CryptoM[EncInt] = for {
    sum <- sumA(zero)(xs)
    n <- encrypt(Additive) { xs.length }
    r <- divide(sum,n)
  } yield r

  def sorted(xs: List[EncInt]): Crypto[List[OpeEnc]] =
    xs.traverse(toOpe).map(_.sorted)

  implicit class DslTraverseOps[F[_]:Traverse](self: F[EncInt]) {
    def sumOpt: Crypto[Option[PaillierEnc]] = deriveDsl.sumOpt(self)
    def productOpt: Crypto[Option[ElGamalEnc]] = deriveDsl.productOpt(self)
  }


  implicit class DslCryptoSyntax[A](self: Crypto[A]) {
    // https://issues.scala-lang.org/browse/SI-1336
    def withFilter(p: A => Boolean): Crypto[A] =
      self.map(x => cond(p(x),x,sys.error("pattern match fail in withFilter of Crypto")))
  }

  implicit class DslCryptoMSyntax[A](self: CryptoM[A]) {
    // https://issues.scala-lang.org/browse/SI-1336
    def withFilter(p: A => Boolean): CryptoM[A] =
      self.map(x => cond(p(x),x,sys.error("pattern match fail in withFilter of CryptoM")))

    def ifM[B](ifTrue: => CryptoM[B])(ifFalse: => CryptoM[B])(
      implicit ev: A === Boolean): CryptoM[B] = {
      val value: CryptoM[Boolean] = ev.subst(self)
      for {
        c <- value
        r <- if (c) ifTrue else ifFalse
      } yield r
    }
  }
  implicit class AnyValLifting[A](self: A) {
    import scalaz.syntax.applicative._
    def lifted: CryptoM[A] = self.point[CryptoM]
  }
}
