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

  override def equals(that: Any) = that match {
    case PaillierEnc(underlying2) => underlying == underlying2
    case _ => false
  }
  override def hashCode = underlying.hashCode
}
class ElGamalEnc(val ca: BigInt, val cb: BigInt, p: BigInt) extends EncInt with Serializable {
  def *(that: ElGamalEnc): ElGamalEnc = (this,that) match {
    case (ElGamalEnc(ca1,ca2),ElGamalEnc(cb1,cb2)) =>
      new ElGamalEnc((ca1 * cb1).mod(p), (ca2 * cb2).mod(p), p)
  }
  override def toString = s"GamalEnc($ca,$cb)"
  override def equals(that: Any) = that match {
    case ElGamalEnc(ca2,cb2) => (ca,cb) == ((ca2,cb2))
    case _ => false
  }
  override def hashCode = ca.hashCode + cb.hashCode * 41
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

sealed trait EncString
case class AesString(underlying: Array[Byte]) extends EncString
case class OpeString(underlying: BigInt) extends EncString

object AesString {
  implicit val aesStringEqual = new Equal[AesString] {
    override def equal(a1: AesString, a2: AesString) = (a1,a2) match {
      case (AesString(x),AesString(y)) => x.size == y.size && (x,y).zipped.forall(_==_)
    }
  }
}

object OpeString {
  implicit val opeStringOrder = new Order[OpeString] {
    override def order(a: OpeString, b: OpeString): Ordering = (a,b) match {
      case (OpeString(x),OpeString(y)) => x ?|? y
    }
  }
  implicit val opeStringOrderScala = opeStringOrder.toScalaOrdering
}
