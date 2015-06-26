package crypto.cipher

import argonaut._
import Argonaut._

import java.security.SecureRandom
import scalaz._

object ElGamal {
  case class Encryptor(f: BigInt => String \/ (BigInt,BigInt))
      extends (BigInt => String \/ (BigInt,BigInt)) {
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: (BigInt,BigInt) => BigInt) extends ((BigInt,BigInt) => BigInt) {
    def apply(x: BigInt, y: BigInt) = f(x,y)
    def apply(xy: (BigInt,BigInt)) = f(xy._1, xy._2)
  }

  case class PubKey(bits: Int, p: BigInt, g: BigInt, h: BigInt, threshold: BigInt)
  object PubKey {
    implicit def codec =
      casecodec5(PubKey.apply,PubKey.unapply)("bits","p","g","h","threshold")
  }

  case class PrivKey(x: BigInt)
  object PrivKey {
    implicit def codec = casecodec1(PrivKey.apply,PrivKey.unapply)("x")
  }

  def create(bits: Int): (Encryptor, Decryptor, PubKey, PrivKey) = {
    val (pub,priv) = Stream.continually(generateKeys(bits)).
      take(10).
      dropWhile(_.isEmpty).
      headOption.
      flatten.
      getOrElse(sys.error("Failed to generate keys."))
    (Encryptor(encrypt(pub)),Decryptor(decrypt(pub,priv)),pub,priv)
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
      (PubKey(bits,p,g,h,BigInt(2).pow(bits/2)), PrivKey(pk))
    }
  }

  def encrypt(pub: PubKey)(input: BigInt): String \/ (BigInt,BigInt) = {
    if (input.bitLength >= pub.p.bitLength / 2) {
      -\/("ElGamal: Input too big")
    } else {
      val rand = BigInt(pub.bits, new SecureRandom)
      val c1 = pub.g.modPow(rand, pub.p)
      val s = pub.h.modPow(rand,pub.p)
      val c2 = (input * s).mod(pub.p)
      \/-((c1,c2))
    }

  }

  private def decrypt(pub: PubKey, priv: PrivKey)(c1: BigInt, c2: BigInt): BigInt = {
    val inverse = c1.modPow(priv.x, pub.p).modInverse(pub.p)
    val plain = (c2 * inverse).mod(pub.p)
    if (plain < pub.threshold) plain else plain - pub.p
  }
}
