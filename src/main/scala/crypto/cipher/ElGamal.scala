package crypto.cipher

import java.security.SecureRandom

object ElGamal {
  case class Encryptor(f: BigInt => (BigInt,BigInt)) extends Function1[BigInt,(BigInt,BigInt)]{
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: (BigInt,BigInt) => BigInt) extends Function2[BigInt,BigInt,BigInt]{
    def apply(x: BigInt, y: BigInt) = f(x,y)
    def apply(xy: (BigInt,BigInt)) = f(xy._1, xy._2)
  }

  case class PubKey(bits: Int, p: BigInt, g: BigInt, h: BigInt)
  case class PrivKey(x: BigInt)

  def create(bits: Int): (Encryptor, Decryptor, PubKey) = {
    val (pub,priv) = Stream.continually(generateKeys(bits)).
      take(10).
      dropWhile(_.isEmpty).
      headOption.
      flatten.
      getOrElse(sys.error("Failed to generate keys."))
    (Encryptor(encrypt(pub)),Decryptor(decrypt(pub,priv)),pub)
  }

  private def generateKeys(bits: Int): Option[(PubKey,PrivKey)] = {
    val rng = new SecureRandom
    val p: BigInt = BigInt.probablePrime(bits, rng)
    val g: BigInt = BigInt.probablePrime(bits, rng)
    val privateKeyOpt: Option[BigInt] = Stream.continually(BigInt.probablePrime(bits,rng)).
      take(100).
      dropWhile(_.gcd(p) != 1).
      headOption

    val hOpt: Option[BigInt] = privateKeyOpt.map(g.modPow(_,p))
    for {
      pk <- privateKeyOpt
      h <- hOpt
    } yield {
      (PubKey(bits,p,g,h), PrivKey(pk))
    }
  }

  def encrypt(pub: PubKey)(input: BigInt): (BigInt,BigInt) = {
    // TODO check if input size is too big
    val rand = BigInt(pub.bits, new SecureRandom)
    val c1 = pub.g.modPow(rand, pub.p)
    val s = pub.h.modPow(rand,pub.p)
    val c2 = (input * s).mod(pub.p)
    (c1,c2)
  }

  private def decrypt(pub: PubKey, priv: PrivKey)(c1: BigInt, c2: BigInt): BigInt = {
    val inverse = c1.modPow(priv.x, pub.p).modInverse(pub.p)
    (c2 * inverse).mod(pub.p)
  }
}
