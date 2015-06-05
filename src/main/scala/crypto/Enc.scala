package crypto

import crypto.cipher._

import scalaz._
import scalaz.std.math.bigInt._
import scalaz.syntax.order._

sealed trait EncInt
class PaillierEnc(val underlying: BigInt, nSquare: BigInt) extends EncInt with Serializable {
  def +(that: PaillierEnc): PaillierEnc = (this,that) match {
    case (PaillierEnc(lhs),PaillierEnc(rhs)) =>
      new PaillierEnc((lhs * rhs).mod(nSquare), nSquare)
  }
  override def toString = s"PaillierEnc($underlying)"
}
class ElGamalEnc(val ca: BigInt, val cb: BigInt, p: BigInt) extends EncInt with Serializable {
  def *(that: ElGamalEnc): ElGamalEnc = (this,that) match {
    case (ElGamalEnc(ca1,ca2),ElGamalEnc(cb1,cb2)) =>
      new ElGamalEnc((ca1 * cb1).mod(p), (ca2 * cb2).mod(p), p)
  }
  override def toString = s"GamalEnc($ca,$cb)"
}
case class AesEnc(underlying: Array[Byte]) extends EncInt
case class OpeEnc(underlying: BigInt) extends EncInt

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

  def unapply(eg: ElGamalEnc): Option[(BigInt,BigInt)] = Some((eg.ca, eg.cb))
  def apply(k: ElGamal.PubKey)(ca: BigInt, cb: BigInt) = new ElGamalEnc(ca, cb, k.p)
}

object AesEnc {
  implicit val aesEncEqual = new Equal[AesEnc] {
    override def equal(a1: AesEnc, a2: AesEnc) = (a1,a2) match {
      case (AesEnc(x),AesEnc(y)) => x.size == y.size && (x,y).zipped.forall(_==_)
    }
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

case class EncString(underlying: Array[Byte])

object EncString {
  implicit val encStringEqual = new Equal[EncString] {
    override def equal(a1: EncString, a2: EncString) = (a1,a2) match {
      case (EncString(x),EncString(y)) => x.size == y.size && (x,y).zipped.forall(_==_)
    }
  }
}
