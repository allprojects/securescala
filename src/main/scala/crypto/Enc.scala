package crypto

import crypto.cipher._

import scalaz._
import scalaz.std.math.bigInt._
import scalaz.syntax.order._

sealed trait Enc
class PaillierEnc(val underlying: BigInt, nSquare: BigInt) extends Enc {
  def +(that: PaillierEnc): PaillierEnc = (this,that) match {
    case (PaillierEnc(lhs),PaillierEnc(rhs)) => new PaillierEnc((lhs * rhs).mod(nSquare), nSquare)
  }
}
case class ElGamalEnc(ca: BigInt, cb: BigInt) extends Enc {
  def *(that: ElGamalEnc): ElGamalEnc = (this,that) match {
    case (ElGamalEnc(ca1,ca2),ElGamalEnc(cb1,cb2)) => ElGamalEnc(ca1 * cb1, ca2 * cb2)
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

  def unapply(p: PaillierEnc): Option[BigInt] = Some(p.underlying)
  def apply(k: Paillier.PubKey)(n: BigInt) = new PaillierEnc(n, k.nSquare)
}

object ElGamalEnc {
  implicit val gamalSemigroup = new Semigroup[ElGamalEnc] {
    def append(f1: ElGamalEnc, f2: => ElGamalEnc): ElGamalEnc = f1*f2
  }
}

object OpeEnc {
  implicit val opeOrder = new Order[OpeEnc] {
    override def order(a: OpeEnc, b: OpeEnc): Ordering = (a,b) match {
      case (OpeEnc(x),OpeEnc(y)) => x ?|? y
    }
  }
  implicit val opeOrderScala = opeOrder.toScalaOrdering
}
