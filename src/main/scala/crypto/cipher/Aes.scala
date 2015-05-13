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

  case class Encryptor(f: Array[Byte] => Array[Byte]) extends (Array[Byte] => Array[Byte]) {
    def apply(x: Array[Byte]) = f(x)
    def apply(x: BigInt) = f(x.toByteArray)
  }

  case class Decryptor(f: Array[Byte] => Array[Byte]) extends (Array[Byte] => Array[Byte]){
    def apply(x: Array[Byte]) = f(x)
  }

  type PrivKey = SecretKey // alias for java's type

  def create(keySize: KeySize): (Encryptor,Decryptor) = {
    val key = generateKey(keySize)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)))
  }

  private def generateKey(keySize: KeySize): PrivKey =
    (KeyGenerator.getInstance("AES") <| (_.init(keySize.bits))).generateKey

  private def encrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (Cipher.getInstance(algorithmString, "SunJCE") <| (_.init(Cipher.ENCRYPT_MODE, priv))).doFinal(input)

  private def decrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (Cipher.getInstance(algorithmString, "SunJCE") <| (_.init(Cipher.DECRYPT_MODE, priv))).doFinal(input)
}
