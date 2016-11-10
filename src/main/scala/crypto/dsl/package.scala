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
    implicit class EncIntInfixOps(self: EncInt) {
      def *(that: EncInt) = multiply(self,that)
      def +(that: EncInt) = add(self,that)
      def -(that: EncInt) = subtract(self,that)
      def /(that: EncInt) = divide(self,that)
      def /!(that: EncInt) = integerDivide(self,that)
      def =:=(that: EncInt) = equal(self,that)
      def ?|?(that: EncInt) = compare(self,that)
      def <(that: EncInt) = compare(self,that).map(_ == LT)
      def <=(that: EncInt) = compare(self,that).map(x => x == LT || x == EQ)
      def >(that: EncInt) = compare(self,that).map(_ == GT)
      def >=(that: EncInt) = compare(self,that).map(x => x == GT || x == EQ)
      def min(that: EncInt) = (self < that).map(cond => if (cond) self else that)
      def max(that: EncInt) = (self > that).map(cond => if (cond) self else that)
    }

    implicit class EncStringInfixOps(self: EncString) {
      def =:=(that: EncString) = equalStr(self,that)
      def ?|?(that: EncString) = compareStr(self,that)
      def ++(that: EncString) = concatStr(self,that)
      def split(regex: String) = splitStr(self,regex)
    }

    implicit class EncRatioOps(self: EncRatio) {
      def ceil = ceilRatio(self)
      def floor = floorRatio(self)
    }
  }
}

trait BaseDsl {
  import crypto.dsl._

  // Applicative Funtor version of the dsl, can be inspected statically
  type Crypto[A] = FreeAp[CryptoF, A]

  // The monadic version of Crypto, CryptoM also allows embedding via Embed
  // therefore we use the Coproduct of CryptoF and Embed
  type CryptoMF[A] = Coproduct[CryptoF,Embed,A]
  type CryptoM[A] = Free[Coproduct[CryptoF,Embed,?], A]

  // Hoist the functor of a Free[CryptoF,_] into the Coproduct to get CryptoM
  def empower[A](p: Free[CryptoF,A]): CryptoM[A] =
    p.mapSuspension(new (CryptoF ~> CryptoMF) {
      def apply[B](fa: CryptoF[B]): CryptoMF[B] = Coproduct(-\/(fa))
    })

  def liftFree[A](fa: CryptoF[A]): CryptoM[A] = {
    val fb: CryptoMF[A] = Coproduct(-\/(fa))
    Free.liftF(fb)
  }

  implicit val cryptomInstance =
    new Functor[CryptoM]
        with Applicative[CryptoM]
        with Monad[CryptoM] {

      def point[A](a: => A) = Free.point[CryptoMF,A](a)
      def bind[A,B](fa: CryptoM[A])(f: A => CryptoM[B]): CryptoM[B] =
        Free.freeMonad[CryptoMF].bind(fa)(f)
  }

  def multiply(lhs: EncInt, rhs: EncInt): Crypto[EncInt] =
    FreeAp.lift(Mult(lhs,rhs,identity))
  def add(lhs: EncInt, rhs: EncInt): Crypto[EncInt] = FreeAp.lift(Plus(lhs,rhs,identity))
  def equal(lhs: EncInt, rhs: EncInt): Crypto[Boolean] =
    FreeAp.lift(Equals(lhs,rhs,identity))
  def equalStr(lhs: EncString, rhs: EncString): Crypto[Boolean] =
    FreeAp.lift(EqualsStr(lhs,rhs,identity))
  def compare(lhs: EncInt, rhs: EncInt): Crypto[Ordering] =
    FreeAp.lift(Compare(lhs,rhs,identity))
  def compareStr(lhs: EncString, rhs: EncString): Crypto[Ordering] =
    FreeAp.lift(CompareStr(lhs,rhs,identity))
  def concatStr(lhs: EncString, rhs: EncString): Crypto[EncString] =
    FreeAp.lift(ConcatStr(lhs,rhs,identity))
  def splitStr(s: EncString, regex: String): Crypto[IList[EncString]] =
    FreeAp.lift(SplitStr(s,regex,identity))

  def addM(lhs: EncInt, rhs: EncInt): CryptoM[EncInt] = liftFree(Plus(lhs,rhs,identity))
  def equalM(lhs: EncInt, rhs: EncInt): CryptoM[Boolean] =
    liftFree(Equals(lhs,rhs,identity))
  def equalStrM(lhs: EncString, rhs: EncString): CryptoM[Boolean] =
    liftFree(EqualsStr(lhs,rhs,identity))
  def compareM(lhs: EncInt, rhs: EncInt): CryptoM[Ordering] =
    liftFree(Compare(lhs,rhs,identity))
  def compareStrM(lhs: EncString, rhs: EncString): CryptoM[Ordering] =
    liftFree(CompareStr(lhs,rhs,identity))
  def concatStrM(lhs: EncString, rhs: EncString): CryptoM[EncString] =
    liftFree(ConcatStr(lhs,rhs,identity))
  def splitStrM(s: EncString, regex: String): CryptoM[IList[EncString]] =
    liftFree(SplitStr(s,regex,identity))
  def multiplyM(lhs: EncInt, rhs: EncInt): CryptoM[EncInt] =
    liftFree(Mult(lhs,rhs,identity))

  def encrypt(s: Scheme)(v: Int): Crypto[EncInt] = FreeAp.lift(Encrypt(s,v,identity))
  def toPaillier(v: EncInt): Crypto[PaillierEnc] = FreeAp.lift(ToPaillier(v,identity))
  def toGamal(v: EncInt): Crypto[ElGamalEnc] = FreeAp.lift(ToGamal(v,identity))
  def toAes(v: EncInt): Crypto[AesEnc] = FreeAp.lift(ToAes(v,identity))
  def toOpe(v: EncInt): Crypto[OpeEnc] = FreeAp.lift(ToOpe(v,identity))
  def toAesStr(v: EncString): Crypto[AesString] = FreeAp.lift(ToAesStr(v,identity))
  def toOpeStr(v: EncString): Crypto[OpeString] = FreeAp.lift(ToOpeStr(v,identity))

  def encryptM(s: Scheme)(v: Int): CryptoM[EncInt] = liftFree(Encrypt(s,v,identity))
  def toPaillierM(v: EncInt): CryptoM[PaillierEnc] = liftFree(ToPaillier(v,identity))
  def toGamalM(v: EncInt): CryptoM[ElGamalEnc] = liftFree(ToGamal(v,identity))
  def toAesM(v: EncInt): CryptoM[AesEnc] = liftFree(ToAes(v,identity))
  def toOpeM(v: EncInt): CryptoM[OpeEnc] = liftFree(ToOpe(v,identity))
  def toAesStrM(v: EncString): CryptoM[AesString] = liftFree(ToAesStr(v,identity))
  def toOpeStrM(v: EncString): CryptoM[OpeString] = liftFree(ToOpeStr(v,identity))

  def ceilRatio(r: EncRatio): Crypto[EncInt] = FreeAp.lift(CeilRatio(r,identity))
  def floorRatio(r: EncRatio): Crypto[EncInt] = FreeAp.lift(FloorRatio(r,identity))

  def ceilRatioM(r: EncRatio): CryptoM[EncInt] = liftFree(CeilRatio(r,identity))
  def floorRatioM(r: EncRatio): CryptoM[EncInt] = liftFree(FloorRatio(r,identity))

  def subtract(lhs: EncInt, rhs: EncInt): Crypto[EncInt] =
    FreeAp.lift(Sub(lhs,rhs,identity))
  def divide(lhs: EncInt, rhs: EncInt): Crypto[EncRatio] =
    FreeAp.lift(Div(lhs,rhs,identity))

  def subtractM(lhs: EncInt, rhs: EncInt): CryptoM[EncInt] =
    liftFree(Sub(lhs,rhs,identity))
  def divideM(lhs: EncInt, rhs: EncInt): CryptoM[EncRatio] =
    liftFree(Div(lhs,rhs,identity))

  def integerDivide(lhs: EncInt, rhs: EncInt): CryptoM[EncInt] = for {
    ratio <- divideM(lhs,rhs)
    result <- floorRatioM(ratio)
  } yield result

  def isEven(v: EncInt): Crypto[Boolean] = FreeAp.lift(IsEven(v,identity))
  def isOdd(v: EncInt): Crypto[Boolean] = FreeAp.lift(IsOdd(v,identity))

  def isEvenM(v: EncInt): CryptoM[Boolean] = liftFree(IsEven(v,identity))
  def isOddM(v: EncInt): CryptoM[Boolean] = liftFree(IsOdd(v,identity))

  def embed[A](p: Crypto[A]): CryptoM[A] = Free.liftF[CryptoMF,A](Coproduct(\/-(new Embed[A]() {
    type I = A
    val v: Crypto[I] = p
    val k: CryptoM[I] => CryptoM[A]= (x: CryptoM[I]) => x
  })))

  def e[A](v: Crypto[A]): CryptoM[A] = embed(v)
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

  def average[F[_]:Traverse](zero: PaillierEnc)(xs: F[EncInt]): CryptoM[EncRatio] = for {
    sum <- sumA(zero)(xs)
    n <- encrypt(Additive) { xs.length }
    r <- divide(sum,n)
  } yield r

  def sorted(xs: List[EncInt]): Crypto[List[OpeEnc]] =
    xs.traverse(toOpe).map(_.sorted)

  def sortBy[A](xs: List[A])(f: A => EncInt): Crypto[List[A]] =
    xs.traverseU(x => toOpe(f(x)).map((x,_))).map(_.sortBy(_._2)).map(_.map(_._1))

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
