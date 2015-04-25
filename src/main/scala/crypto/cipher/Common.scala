package crypto.cipher

sealed trait Enc
case class PaillierEnc(underlying: BigInt) extends Enc
case class GamalEnc(underlying: BigInt) extends Enc
case class AESEnc(underlying: BigInt) extends Enc
case class OPEEnc(underlying: BigInt) extends Enc
case class NoEnc(underlying: BigInt) extends Enc

sealed trait Scheme
case object Additive extends Scheme
case object Multiplicative extends Scheme
case object Deterministic extends Scheme
case object OrderPreserving extends Scheme
case object NoEncScheme extends Scheme

// Keychains
case class EncKeys(paillier: Paillier.PubKey)
case class DecKeys(paillier: Paillier.Decryptor)

object Common {
  def decrypt(keys: DecKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case GamalEnc(x) => ???
    case AESEnc(x) => ???
    case OPEEnc(x) => ???
    case NoEnc(x) => x
  }

  def encrypt(s: Scheme, keys: EncKeys): BigInt => Enc = input => s match {
    case Additive => PaillierEnc(Paillier.encrypt(keys.paillier)(input))
    case Multiplicative => ???
    case Deterministic => ???
    case OrderPreserving => ???
    case NoEncScheme => NoEnc(input)
  }

  // local version
  def convert(encKeys: EncKeys, decKeys: DecKeys): (Scheme, Enc) => Enc = {
    case (Additive,in@PaillierEnc(_)) => in
    case (Multiplicative,in@GamalEnc(_)) => in
    case (Deterministic,in@AESEnc(_)) => in
    case (OrderPreserving,in@OPEEnc(_)) => in
    case (NoEncScheme,in@NoEnc(_)) => in
    case (s,input) => (encrypt(s, encKeys) compose decrypt(decKeys))(input)
  }

}
