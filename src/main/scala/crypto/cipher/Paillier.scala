package crypto.cipher

import scala.util._

case class Encryptor(f: BigInt => BigInt) extends Function1[BigInt,BigInt]{
  def apply(x: BigInt) = f(x)
}

case class Decryptor(f: BigInt => BigInt) extends Function1[BigInt,BigInt]{
  def apply(x: BigInt) = f(x)
}

object Paillier {
  case class PubKey(bits: Int, n: BigInt, g: BigInt, nSquare: BigInt)
  case class PrivKey(lambda: BigInt, mu: BigInt)

  def create(bits: Int): (Encryptor,Decryptor,PubKey) = {
    val (pub,priv) =
      Stream.continually(Paillier.generateKeys(bits)).dropWhile(_.isEmpty).head.get

    (Encryptor(Paillier.encrypt(pub)),Decryptor(Paillier.decrypt(pub,priv)), pub)
  }

  private def generateKeys(bits: Int): Option[(PubKey,PrivKey)] = {
    val rand = new Random
    val p = BigInt(bits/2, 64, rand)
    val q = BigInt(bits/2, 64, rand)

    val n = p * q
    val nSquare = n*n

    val g: BigInt = n + 1
    val lambda = lcm(p-1,q-1)

    Try {
      // Implicit check if multiplicative inverse exists, because then modInverse fails
      val mu = ((g.modPow(lambda, nSquare) - 1) / n).modInverse(n)
      (PubKey(bits,n,g,nSquare),PrivKey(lambda, mu))
    }.toOption
  }

  def encrypt(pub: PubKey)(input: BigInt): BigInt = {
    val r = BigInt(pub.bits, new Random)
    pub.g.modPow(input, pub.nSquare) * r.modPow(pub.n,pub.nSquare) mod pub.nSquare
  }

  private def decrypt(pub: PubKey, priv: PrivKey)(input: BigInt): BigInt = {
    (functionL(pub.n)(input.modPow(priv.lambda, pub.nSquare)) * priv.mu) mod pub.n
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
