package crypto.cipher

import crypto.{DecKeys,EncKeys}

sealed trait Enc
case class PaillierEnc(underlying: BigInt) extends Enc
case class GamalEnc(ca: BigInt, cb: BigInt) extends Enc {
  def *(that: GamalEnc): GamalEnc = (this,that) match {
    case (GamalEnc(ca1,ca2),GamalEnc(cb1,cb2)) => GamalEnc(ca1 * cb1, ca2 * cb2)
  }
}
case class AESEnc(underlying: BigInt) extends Enc
case class OPEEnc(underlying: BigInt) extends Enc
case class NoEnc(underlying: BigInt) extends Enc

sealed trait Scheme
case object Additive extends Scheme
case object Multiplicative extends Scheme
case object Deterministic extends Scheme
case object OrderPreserving extends Scheme
case object NoEncScheme extends Scheme

object Common {
  def decrypt(keys: DecKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case GamalEnc(x,y) => keys.gamal(x,y)
    case AESEnc(x) => ???
    case OPEEnc(x) => ???
    case NoEnc(x) => x
  }

  def encrypt(s: Scheme, keys: EncKeys): BigInt => Enc = input => s match {
    case Additive => PaillierEnc(Paillier.encrypt(keys.paillier)(input))
    case Multiplicative => GamalEnc.tupled(ElGamal.encrypt(keys.gamal)(input))
    case Deterministic => ???
    case OrderPreserving => ???
    case NoEncScheme => NoEnc(input)
  }

  // local version
  def convert(encKeys: EncKeys, decKeys: DecKeys): (Scheme, Enc) => Enc = {
    case (Additive,in@PaillierEnc(_)) => in
    case (Multiplicative,in@GamalEnc(_,_)) => in
    case (Deterministic,in@AESEnc(_)) => in
    case (OrderPreserving,in@OPEEnc(_)) => in
    case (NoEncScheme,in@NoEnc(_)) => in
    case (s,input) => (encrypt(s, encKeys) compose decrypt(decKeys))(input)
  }

}
