package crypto.cipher

import java.security.SecureRandom

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

  case class Encryptor(f: BigInt => BigInt) extends Function1[BigInt,BigInt]{
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

  def encrypt(priv: PrivKey)(input: BigInt): BigInt =
    BigInt(instance.nativeEncrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits))

  def decrypt(priv: PrivKey)(input: BigInt): BigInt =
    BigInt(instance.nativeDecrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits))


}
