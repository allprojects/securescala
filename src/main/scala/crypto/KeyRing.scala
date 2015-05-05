package crypto

import crypto.cipher._

case class KeyRing(pub: PubKeys, priv: PrivKeys)
case class PubKeys(paillier: Paillier.PubKey, gamal: ElGamal.PubKey)
case class PrivKeys(
  paillier: Paillier.Decryptor,
  gamal: ElGamal.Decryptor,
  aesEnc: Aes.Encryptor,
  aesDec: Aes.Decryptor,
  opeIntEnc: Ope.Encryptor,
  opeIntDec: Ope.Decryptor,
  opePriv: Ope.PrivKey)

object KeyRing {
  def create: KeyRing = {
    val (_, paillierDec, paillierPub) = Paillier.create(1024)
    val (_, gamalDec, gamalPub) = ElGamal.create(1024)
    val (aesEnc, aesDec) = Aes.create(Aes.B256)
    val (opeIntEnc, opeIntDec,opePriv) = Ope.createNum(128)

    val encKeys = PubKeys(paillierPub, gamalPub)
    val decKeys = PrivKeys(paillierDec, gamalDec, aesEnc, aesDec, opeIntEnc, opeIntDec, opePriv)
    KeyRing(encKeys,decKeys)
  }
}
