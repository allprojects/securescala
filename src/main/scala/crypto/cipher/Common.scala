package crypto.cipher

import crypto._

import scalaz._

sealed trait Scheme { type Out }
sealed trait AsymmetricScheme extends Scheme

case object Additive extends AsymmetricScheme { type Out = PaillierEnc}
case object Multiplicative extends AsymmetricScheme { type Out = ElGamalEnc }

case object Equality extends Scheme { type Out = AesEnc }
case object Comparable extends Scheme { type Out = OpeEnc }

object Common {
  def decrypt(keys: PrivKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case ElGamalEnc(x,y) => keys.gamal(x,y)
    case AesEnc(x) => BigInt(keys.aesDec(x))
    case OpeEnc(x) => keys.opeIntDec(x)
  }

  def depEncryptPub(s: AsymmetricScheme, keys: PubKeys): BigInt => s.Out =
    input => encryptPub(s,keys)(input).map(_.asInstanceOf[s.Out]).valueOr(sys.error)

  def encryptPub(s: AsymmetricScheme, keys: PubKeys): BigInt => String \/ Enc =
    input => s match {
      case Additive => Paillier.encrypt(keys.paillier)(input).map(PaillierEnc(keys.paillier))
      case Multiplicative =>
        ElGamal.encrypt(keys.gamal)(input).map { case (x,y) => ElGamalEnc(keys.gamal)(x,y)}
    }

  def depEncrypt(s: Scheme, keys: KeyRing): BigInt => s.Out =
    input => encryptChecked(s,keys)(input).valueOr(sys.error).asInstanceOf[s.Out]

  def encrypt(s: Scheme, keys: KeyRing): BigInt => Enc =
    input => encryptChecked(s,keys)(input).valueOr(sys.error)

  def encryptChecked(s: Scheme, keys: KeyRing): BigInt => String \/ Enc = input => s match {
    case Additive => encryptPub(Additive, keys.pub)(input)
    case Multiplicative => encryptPub(Multiplicative, keys.pub)(input)
    case Equality => \/-(AesEnc(keys.priv.aesEnc(input)))
    case Comparable => keys.priv.opeIntEnc(input).map(OpeEnc(_))
  }

  def safeConvert(keys: KeyRing): (Scheme, Enc) => String \/ Enc = {
    // Nothing to do
    case (Additive,in@PaillierEnc(_)) => \/-(in)
    case (Multiplicative,in@ElGamalEnc(_,_)) => \/-(in)
    case (Equality,in@AesEnc(_)) => \/-(in)
    case (Comparable,in@OpeEnc(_)) => \/-(in)

    // Conversion required
    case (Additive,in) => (encryptPub(Additive, keys.pub) compose decrypt(keys.priv))(in)
    case (Multiplicative,in) => (encryptPub(Multiplicative, keys.pub) compose decrypt(keys.priv))(in)
    case (Equality,in) => \/-(AesEnc(keys.priv.aesEnc(decrypt(keys.priv)(in))))
    case (Comparable,in) => keys.priv.opeIntEnc(decrypt(keys.priv)(in)).map(OpeEnc(_))
  }

  def convert(keys: KeyRing): (Scheme, Enc) => Enc =
    (scheme,enc) => safeConvert(keys)(scheme,enc).valueOr(sys.error)

  def depConvert(keys: KeyRing)(s: Scheme, in: Enc): s.Out =
    convert(keys)(s,in).asInstanceOf[s.Out]

  def zero(keys: PubKeys): PaillierEnc = depEncryptPub(Additive, keys)(0)
  def one(keys: PubKeys): ElGamalEnc = depEncryptPub(Multiplicative, keys)(1)
}
