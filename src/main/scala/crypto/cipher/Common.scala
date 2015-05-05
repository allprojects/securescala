package crypto.cipher

import crypto._


sealed trait Scheme
sealed trait AsymmetricScheme extends Scheme

case object Additive extends AsymmetricScheme
case object Multiplicative extends AsymmetricScheme

case object Equality extends Scheme
case object Comparable extends Scheme

object Common {
  def decrypt(keys: PrivKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case GamalEnc(x,y) => keys.gamal(x,y)
    case AesEnc(x) => BigInt(keys.aesDec(x))
    case OpeEnc(x) => keys.opeIntDec(x)
  }

  def encrypt(s: AsymmetricScheme, keys: PubKeys): BigInt => Enc = input => s match {
    case Additive => PaillierEnc(Paillier.encrypt(keys.paillier)(input))
    case Multiplicative => (GamalEnc.apply _).tupled(ElGamal.encrypt(keys.gamal)(input))
  }

  def convert(keys: KeyRing): (Scheme, Enc) => Enc = {
    case (Additive,in@PaillierEnc(_)) => in
    case (Multiplicative,in@GamalEnc(_,_)) => in
    case (Equality,in@AesEnc(_)) => in
    case (Comparable,in@OpeEnc(_)) => in
    case (Additive,in) => (encrypt(Additive, keys.pub) compose decrypt(keys.priv))(in)
    case (Multiplicative,in) => (encrypt(Multiplicative, keys.pub) compose decrypt(keys.priv))(in)
    case (Equality,in) => AesEnc.apply(keys.priv.aesEnc(decrypt(keys.priv)(in)))
    case (s,input) => ???
  }

}
