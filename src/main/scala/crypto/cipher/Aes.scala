package crypto.cipher

import argonaut._
import Argonaut._

import javax.crypto.Cipher
import javax.crypto.KeyGenerator
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec
import scalaz.syntax.IdOps
import scalaz.syntax.id._

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

  case class PrivKey(unwrap: SecretKey)
  object PrivKey {
    implicit def encode: EncodeJson[PrivKey] = EncodeJson((key: PrivKey) =>
      ("secretkey" := key.unwrap.getEncoded.toList.map(_.toInt)) ->: jEmptyObject
    )
    implicit def decode: DecodeJson[PrivKey] = DecodeJson(c =>
      (c --\ "secretkey").as[List[Int]].map(bytes =>
        PrivKey(new SecretKeySpec(bytes.map(_.toByte).toArray, "AES")))
    )
  }

  def create(keySize: KeySize): (Encryptor,Decryptor,PrivKey) = {
    val key = generateKey(keySize)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)),key)
  }

  def fromKey(key: PrivKey) = (Encryptor(encrypt(key)), Decryptor(decrypt(key)))

  private def generateKey(keySize: KeySize): PrivKey =
    PrivKey((KeyGenerator.getInstance("AES") <| (_.init(keySize.bits))).generateKey)

  private def encrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (Cipher.getInstance(algorithmString, "SunJCE") <| (_.init(Cipher.ENCRYPT_MODE, priv.unwrap))).doFinal(input)

  private def decrypt(priv: PrivKey)(input: Array[Byte]): Array[Byte] =
    (Cipher.getInstance(algorithmString, "SunJCE") <| (_.init(Cipher.DECRYPT_MODE, priv.unwrap))).doFinal(input)
}
