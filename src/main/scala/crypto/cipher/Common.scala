package crypto.cipher

sealed trait Enc
case class PaillierEnc(underlying: BigInt) extends Enc
case class NoEnc(underlying: BigInt) extends Enc // stub, will be removed

// Keychains
case class EncKeys(paillier: Paillier.PubKey)
case class DecKeys(paillier: Paillier.Decryptor)

object Common {
  def decrypt(keys: DecKeys): Enc => BigInt = _ match {
    case PaillierEnc(x) => keys.paillier(x)
    case NoEnc(x) => x
  }

  def encrypt(s: Scheme, keys: EncKeys): BigInt => Enc = input => s match {
    case PaillierS => PaillierEnc(Paillier.encrypt(keys.paillier)(input))
    case NoEncS => NoEnc(input)
  }

  // local version
  def convert(encKeys: EncKeys, decKeys: DecKeys): (Scheme, Enc) => Enc = {
    case (PaillierS,in@PaillierEnc(_)) => in
    case (NoEncS,in@NoEnc(_)) => in
    case (s,input) => (encrypt(s, encKeys) compose decrypt(decKeys))(input)
  }

  sealed trait Scheme
  case object PaillierS extends Scheme
  case object NoEncS extends Scheme
}
