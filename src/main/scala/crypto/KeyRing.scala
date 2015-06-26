package crypto

import crypto.cipher._
import argonaut._
import Argonaut._

case class KeyRing(pub: PubKeys, priv: PrivKeys)
case class PubKeys(paillier: Paillier.PubKey, elgamal: ElGamal.PubKey)
case class PrivKeys(
  paillier: Paillier.Decryptor,
  elgamal: ElGamal.Decryptor,
  aesEnc: Aes.Encryptor,
  aesDec: Aes.Decryptor,
  opeIntEnc: OpeInt.Encryptor,
  opeIntDec: OpeInt.Decryptor,
  opeIntPriv: OpeInt.PrivKey,
  opeStrEnc: OpeStr.Encryptor,
  opeStrDec: OpeStr.Decryptor,
  opeStrPriv: OpeStr.PrivKey
)

object KeyRing {
  implicit def narrowToPublicKeys: KeyRing => PubKeys = _.pub
  implicit def narrowToPrivateKeys: KeyRing => PrivKeys = _.priv
  def create: KeyRing = {
    val (_, paillierDec, paillierPub) = Paillier.create(1024)
    val (_, elgamalDec, elgamalPub) = ElGamal.create(1024)
    val (aesEnc, aesDec) = Aes.create(Aes.B256)
    val (opeIntEnc, opeIntDec,opeIntPriv) = OpeInt.create(128)
    val (opeStrEnc, opeStrDec,opeStrPriv) = OpeStr.create(128, 10)

    val encKeys = PubKeys(paillierPub, elgamalPub)
    val decKeys = PrivKeys(
      paillierDec, elgamalDec, aesEnc, aesDec,
      opeIntEnc, opeIntDec, opeIntPriv,
      opeStrEnc, opeStrDec, opeStrPriv
    )
    KeyRing(encKeys,decKeys)
  }
}

object PubKeys {
  implicit def codec: CodecJson[PubKeys] =
    casecodec2(PubKeys.apply,PubKeys.unapply)("paillier","elgamal")
}
