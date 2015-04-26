package crypto

import crypto.cipher._

case class KeyRing(enc: EncKeys, dec: DecKeys)
case class EncKeys(paillier: Paillier.PubKey, gamal: ElGamal.PubKey)
case class DecKeys(paillier: Paillier.Decryptor, gamal: ElGamal.Decryptor)

object KeyRing {
  def create: KeyRing = {
    val (_, paillierDec, paillierPub) = Paillier.create(1024)
    val (_, gamalDec, gamalPub) = ElGamal.create(1024)

    val encKeys = EncKeys(paillierPub, gamalPub)
    val decKeys = DecKeys(paillierDec, gamalDec)
    KeyRing(encKeys,decKeys)
  }
}
