package crypto.cipher

import argonaut._
import Argonaut._
import java.security.SecureRandom
import scala.util._
import scalaz._

object Paillier {
  case class Encryptor(f: BigInt => String \/ BigInt) extends (BigInt => String \/ BigInt) {
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: BigInt => BigInt) extends (BigInt => BigInt){
    def apply(x: BigInt) = f(x)
  }

  case class PubKey(
    bits: Int,
    n: BigInt,
    g: BigInt,
    nSquare: BigInt,
    threshold: BigInt
  )

  object PubKey {
    implicit def codec =
      casecodec5(PubKey.apply,PubKey.unapply)("bits","n","g","nSquare","threshold")
  }

  case class PrivKey(lambda: BigInt, mu: BigInt)

  object PrivKey {
    implicit def codec = casecodec2(PrivKey.apply,PrivKey.unapply)("lambda","mu")
  }

  def create(bits: Int): (Encryptor,Decryptor,PubKey,PrivKey) = {
    val (pub,priv) = Stream.continually(Paillier.generateKeys(bits)).
      take(100).
      dropWhile(_.isEmpty).
      headOption.
      flatten.
      getOrElse(sys.error("Failed to generate keys."))

    (Encryptor(encrypt(pub)),Decryptor(decrypt(pub,priv)), pub, priv)
  }

  private def generateKeys(bits: Int): Option[(PubKey,PrivKey)] = {
    val rand = new SecureRandom
    val p = BigInt(bits/2, 64, rand)
    val q = BigInt(bits/2, 64, rand)

    val n = p * q
    val nSquare = n*n

    val g: BigInt = n + 1
    val lambda = lcm(p-1,q-1)

    Try {
      // Implicit check if multiplicative inverse exists, because then modInverse fails
      val mu = ((g.modPow(lambda, nSquare) - 1) / n).modInverse(n)
      (PubKey(bits,n,g,nSquare,BigInt(2).pow(bits/2)),PrivKey(lambda, mu))
    }.toOption
  }

  def encrypt(pub: PubKey)(input: BigInt): String \/ BigInt = {
    if (input >= pub.nSquare) {
      -\/("Paillier: Input too big")
    } else {
      val r = BigInt(pub.bits, new Random)
      \/-(pub.g.modPow(input, pub.nSquare) * r.modPow(pub.n,pub.nSquare) mod pub.nSquare)
    }
  }

  private def decrypt(pub: PubKey, priv: PrivKey)(input: BigInt): BigInt = {
    val plain =
      (functionL(pub.n)(input.modPow(priv.lambda, pub.nSquare)) * priv.mu) mod pub.n
    if (plain < pub.threshold) plain else plain - pub.n
  }

  private def functionL(n: BigInt)(u: BigInt): BigInt = (u - 1) / n

  private def lcm(a: BigInt, b: BigInt): BigInt = {
    if (a == BigInt(0) && b == BigInt(0)) {
      0
    } else {
      (a.abs / a.gcd(b)) * b.abs
    }
  }
}
