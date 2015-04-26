package crypto.cipher

import scalaz.syntax.IdOps
import scalaz.syntax.id._

import javax.crypto.SecretKey
import javax.crypto.KeyGenerator
import javax.crypto.Cipher;

object Aes {
  sealed trait KeySize { def bits: Int }
  case object B128 extends KeySize { def bits = 128 }
  case object B192 extends KeySize { def bits = 192 }
  case object B256 extends KeySize { def bits = 256 }

  private val algorithmString = "AES/ECB/PKCS5Padding"

  private lazy val cipherEncrypt: Cipher =
    Cipher.getInstance(algorithmString, "SunJCE")

  private lazy val cipherDecrypt: Cipher =
    Cipher.getInstance(algorithmString, "SunJCE")

  case class Encryptor(f: Array[Byte] => Array[Byte]) extends Function1[Array[Byte],Array[Byte]]{
    def apply(x: Array[Byte]) = f(x)
  }

  case class Decryptor(f: Array[Byte] => Array[Byte]) extends Function1[Array[Byte],Array[Byte]]{
    def apply(x: Array[Byte]) = f(x)
  }

  type PrivKey = SecretKey

  def create(keySize: KeySize): (Encryptor,Decryptor) = {
    val key = generateKey(keySize)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)))
  }

  def generateKey(keySize: KeySize): SecretKey =
    (KeyGenerator.getInstance("AES") <| (_.init(keySize.bits))).generateKey

  private def encrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (cipherEncrypt <| (_.init(Cipher.ENCRYPT_MODE, priv))).doFinal(input)

  private def decrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (cipherDecrypt <| (_.init(Cipher.DECRYPT_MODE, priv))).doFinal(input)
}
