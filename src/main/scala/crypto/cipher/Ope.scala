package crypto.cipher

import java.security.SecureRandom

trait Ope {
  private val plainTextBits = 64
  private val cipherTextBits = 96

  case class PrivKey(key: String, bits: Int)

  private def generateKey(bits: Int): PrivKey = {
    PrivKey(BigInt(bits, new SecureRandom).toString(32), bits)
  }

  @native
  def native_encrypt(password: String, plaintext: String,
    plainTextBits: Int, cipherTextBits: Int): String

  @native
  def native_decrypt(password: String, ciphertext: String, plainTextBits: Int, cipherTextBits: Int): String
}

object Ope extends Ope
