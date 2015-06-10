package crypto.cipher

import java.security.SecureRandom
import scala.util.Try
import scala.collection.immutable.NumericRange

import scalaz._

class OpeNative { // `class` required for javah/native interface
  @native def nativeEncrypt(
    password: String, plaintext: String, plainTextBits: Int, cipherTextBits: Int): String

  @native def nativeDecrypt(
    password: String, ciphertext: String, plainTextBits: Int, cipherTextBits: Int): String
}

object OpeNative {
  val home = System.getProperty("user.home")
  System.load(home + "/libope.so")
  val instance = new OpeNative
}

object Ope {
  private val numPlainTextBits = 64
  private val numCipherTextBits = 96

  case class Encryptor(f: BigInt => (String \/ BigInt))
      extends (BigInt => String \/ BigInt) {
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: BigInt => BigInt) extends (BigInt => BigInt) {
    def apply(x: BigInt) = f(x)
  }

  case class PrivKey(
    key: String,
    bits: Int,
    plainBits: Int,
    cipherBits: Int,
    domain: CipherDomain[BigInt]
  )

  def createNum(bits: Int): (Encryptor, Decryptor, PrivKey) = {
    val key = generateKey(bits, numPlainTextBits, numCipherTextBits)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)), key)
  }

  private def generateKey(bits: Int, plainBits: Int, cipherBits: Int): PrivKey = {
    val limit = BigInt(2).pow(plainBits) / 2
    val dom = CipherDomain(-limit,limit-1)
    PrivKey(BigInt(bits, new SecureRandom).toString(32), bits, plainBits, cipherBits, dom)
  }

  def encrypt(priv: PrivKey)(input: BigInt): String \/ BigInt = input match {
    case x if !priv.domain.contains(x) => -\/("OPE: Input out of range")
    case x => \/.fromTryCatchNonFatal { this.synchronized(
      OpeNative.instance.nativeEncrypt(
        priv.key,
        (input+priv.domain.max+1).toString,
        priv.plainBits,
        priv.cipherBits))
    }.leftMap(e => "OPE: " + e.getMessage).map(BigInt(_))
  }

  def decrypt(priv: PrivKey)(input: BigInt): BigInt = this.synchronized {
    val plain = BigInt(
      OpeNative.instance.nativeDecrypt(
        priv.key, input.toString, priv.plainBits, priv.cipherBits))

    plain - (priv.domain.max+1)
  }
}

object OpeStr {
}
