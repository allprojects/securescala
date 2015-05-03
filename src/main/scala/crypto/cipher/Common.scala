package crypto.cipher

import crypto._

sealed trait Scheme
case object Additive extends Scheme
case object Multiplicative extends Scheme
case object NoEncScheme extends Scheme

object Common {
  def decrypt(keys: PrivKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case GamalEnc(x,y) => keys.gamal(x,y)
    case AesEnc(x) => BigInt(keys.aesDec(x))
    case OpeEnc(x) => keys.opeIntDec(x)
    case NoEnc(x) => x
  }

  def encrypt(s: Scheme, keys: PubKeys): BigInt => Enc = input => s match {
    case Additive => PaillierEnc(Paillier.encrypt(keys.paillier)(input))
    case Multiplicative => (GamalEnc.apply _).tupled(ElGamal.encrypt(keys.gamal)(input))
    case NoEncScheme => NoEnc(input)
  }

  // local version
  def convert(encKeys: PubKeys, decKeys: PrivKeys): (Scheme, Enc) => Enc = {
    case (Additive,in@PaillierEnc(_)) => in
    case (Multiplicative,in@GamalEnc(_,_)) => in
    case (NoEncScheme,in@NoEnc(_)) => in
    case (s,input) => (encrypt(s, encKeys) compose decrypt(decKeys))(input)
  }

}
