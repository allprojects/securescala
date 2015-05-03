package crypto

import scalaz._
import scalaz.std.math.bigInt._
import scalaz.syntax.order._

sealed trait Enc
case class PaillierEnc(underlying: BigInt) extends Enc {
  // TODO: modulus public key nSquare
  def +(that: PaillierEnc): PaillierEnc = (this,that) match {
    case (PaillierEnc(lhs),PaillierEnc(rhs)) => PaillierEnc(lhs * rhs)
  }
}
case class GamalEnc(ca: BigInt, cb: BigInt) extends Enc {
  // TODO: modulus public key
  def *(that: GamalEnc): GamalEnc = (this,that) match {
    case (GamalEnc(ca1,ca2),GamalEnc(cb1,cb2)) => GamalEnc(ca1 * cb1, ca2 * cb2)
  }
}
case class AesEnc(underlying: Array[Byte]) extends Enc {
  def =:=(that: AesEnc): Boolean = (this,that) match {
    case (AesEnc(x),AesEnc(y)) => x.size == y.size && (x,y).zipped.forall(_==_)
  }
}
case class OpeEnc(underlying: BigInt) extends Enc

object PaillierEnc {
  implicit val paillierSemigroup = new Semigroup[PaillierEnc] {
    def append(f1: PaillierEnc, f2: => PaillierEnc): PaillierEnc = f1+f2
  }
}

object GamalEnc {
  implicit val gamalSemigroup = new Semigroup[GamalEnc] {
    def append(f1: GamalEnc, f2: => GamalEnc): GamalEnc = f1*f2
  }
}

object OpeEnc {
  implicit val opeOrder = new Order[OpeEnc] {
    override def order(a: OpeEnc, b: OpeEnc): Ordering = (a,b) match {
      case (OpeEnc(x),OpeEnc(y)) => x ?|? y
    }
  }
}
