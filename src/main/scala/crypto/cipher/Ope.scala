package crypto.cipher

import java.security.SecureRandom

trait Ope { // trait required for javah/native interface
  private val numPlainTextBits = 64
  private val numCipherTextBits = 96

  case class Encryptor(f: BigInt => String) extends Function1[BigInt,String]{
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: String => BigInt) extends Function1[String,BigInt]{
    def apply(x: String) = f(x)
  }

  case class PrivKey(key: String, bits: Int, plainBits: Int, cipherBits: Int)

  def createNum(bits: Int): (Encryptor,Decryptor) = {
    val key = generateKey(bits, numPlainTextBits, numCipherTextBits)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)))
  }

  private def generateKey(bits: Int, plainBits: Int, cipherBits: Int): PrivKey =
    PrivKey(BigInt(bits, new SecureRandom).toString(32), bits, plainBits, cipherBits)

  @native
  def native_encrypt(password: String, plaintext: String,
    plainTextBits: Int, cipherTextBits: Int): String

  @native
  def native_decrypt(password: String, ciphertext: String, plainTextBits: Int, cipherTextBits: Int): String

  private def encrypt(priv: PrivKey)(input: BigInt): String =
    native_encrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits)

  private def decrypt(priv: PrivKey)(input: String): BigInt =
    BigInt(native_decrypt(priv.key, input, priv.plainBits, priv.cipherBits))
}

object Ope extends Ope

object OpeTest extends App {
  val (enc,dec) = Ope.createNum(128)

  val plain = BigInt("19980902")
  println(plain)
  println(enc(plain))
  println(dec(enc(plain)))
}
