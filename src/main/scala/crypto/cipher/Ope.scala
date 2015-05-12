package crypto.cipher

import java.security.SecureRandom
import scala.util.Try

import scalaz._

class OpeNative { // `class` required for javah/native interface
  @native def nativeEncrypt(
    password: String, plaintext: String, plainTextBits: Int, cipherTextBits: Int): String

  @native def nativeDecrypt(
    password: String, ciphertext: String, plainTextBits: Int, cipherTextBits: Int): String
}

object Ope {
  val home = System.getProperty("user.home")
  System.load(home + "/libope.so")

  private val numPlainTextBits = 64
  private val numCipherTextBits = 96

  private val instance = new OpeNative

  case class Encryptor(f: BigInt => (String \/ BigInt)) extends Function1[BigInt,String \/ BigInt]{
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: BigInt => BigInt) extends Function1[BigInt,BigInt]{
    def apply(x: BigInt) = f(x)
  }

  case class PrivKey(key: String, bits: Int, plainBits: Int, cipherBits: Int)

  def createNum(bits: Int): (Encryptor, Decryptor, PrivKey) = {
    val key = generateKey(bits, numPlainTextBits, numCipherTextBits)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)), key)
  }

  private def generateKey(bits: Int, plainBits: Int, cipherBits: Int): PrivKey =
    PrivKey(BigInt(bits, new SecureRandom).toString(32), bits, plainBits, cipherBits)

  def encrypt(priv: PrivKey)(input: BigInt): String \/ BigInt = input match {
    case x if x < -(BigInt(2).pow(priv.plainBits-1)) => -\/("OPE: Input is too small")
    case x if x > BigInt(2).pow(priv.plainBits-1) - 1 => -\/("OPE: Input is too big")
    case x =>
      val encrypted = Try {
        instance.nativeEncrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits)
      } match {
        case scala.util.Success(enc) => \/-(enc)
        case scala.util.Failure(e) => -\/(e)
      }
      encrypted.leftMap(e => "OPE: " + e.getMessage).map(BigInt(_))
  }

  def decrypt(priv: PrivKey)(input: BigInt): BigInt =
    BigInt(instance.nativeDecrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits))

}
