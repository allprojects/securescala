package crypto

import crypto.cipher._
import argonaut._
import Argonaut._
import com.cedarsoftware.util.io._
case class KeyRing(pub: PubKeys, priv: PrivKeys)
case class PubKeys(paillier: Paillier.PubKey, elgamal: ElGamal.PubKey)
case class PrivKeys(
  paillier: Paillier.Decryptor,
  paillierPriv: Paillier.PrivKey,
  elgamal: ElGamal.Decryptor,
  elgamalPriv: ElGamal.PrivKey,
  aesEnc: Aes.Encryptor,
  aesDec: Aes.Decryptor,
  aesPriv: Aes.PrivKey,
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
    val (_, paillierDec, paillierPub, paillierPriv) = Paillier.create(1024)
    val (_, elgamalDec, elgamalPub, elgamalPriv) = ElGamal.create(1024)
    val (aesEnc, aesDec, aesPriv) = Aes.create(Aes.B256)
    val (opeIntEnc, opeIntDec,opeIntPriv) = OpeInt.create(128)
    val (opeStrEnc, opeStrDec,opeStrPriv) = OpeStr.create(128, 10)

    val encKeys = PubKeys(paillierPub, elgamalPub)
    val decKeys = PrivKeys(
      paillierDec, paillierPriv,
      elgamalDec, elgamalPriv,
      aesEnc, aesDec, aesPriv,
      opeIntEnc, opeIntDec, opeIntPriv,
      opeStrEnc, opeStrDec, opeStrPriv
    )
    KeyRing(encKeys,decKeys)
  }

  implicit def codec: CodecJson[KeyRing] = CodecJson[KeyRing](
    (k: KeyRing) => ("pubs" := k.pub) ->: ("privs" := k.priv) ->: jEmptyObject,
    c => for {
      pub <- (c --\ "pubs").as[PubKeys]
      priv <- (c --\ "privs").as[PrivKeys](PrivKeys.decode(pub))
    } yield KeyRing(pub,priv)
   )
}

object PrivKeys {
  implicit def encode: EncodeJson[PrivKeys] = {
    EncodeJson((p: PrivKeys) =>
      ("paillier_priv" := p.paillierPriv) ->:
        ("elgamal_priv" := p.elgamalPriv) ->:
        ("aes_priv" := JsonWriter.objectToJson(p.aesPriv)) ->:
        ("opeInt_priv" := p.opeIntPriv) ->:
        ("opeStr_priv" := p.opeStrPriv) ->:
        jEmptyObject)
  }

  implicit def decode(pub: PubKeys): DecodeJson[PrivKeys] = DecodeJson(c => for {
    paillierPriv <- (c --\ "paillier_priv").as[Paillier.PrivKey]
    elgamalPriv <- (c --\ "elgamal_priv").as[ElGamal.PrivKey]
    aesPriv <-
      (c --\ "aes_priv").as[String].map(JsonReader.jsonToJava(_).asInstanceOf[Aes.PrivKey])
    opeIntPriv <- (c --\ "opeInt_priv").as[OpeInt.PrivKey]
    opeStrPriv <- (c --\ "opeStr_priv").as[OpeStr.PrivKey]
  } yield {
    val (_,paillierDec) = Paillier.fromKeys(pub.paillier,paillierPriv)
    val (_,elgamalDec) = ElGamal.fromKeys(pub.elgamal,elgamalPriv)
    val (aesEnc,aesDec) = Aes.fromKey(aesPriv)
    val (opeIntEnc,opeIntDec) = OpeInt.fromKey(opeIntPriv)
    val (opeStrEnc,opeStrDec) = OpeStr.fromKey(opeStrPriv)
    PrivKeys(
      paillierDec,
      paillierPriv,

      elgamalDec,
      elgamalPriv,

      aesEnc,
      aesDec,
      aesPriv,

      opeIntEnc,
      opeIntDec,
      opeIntPriv,

      opeStrEnc,
      opeStrDec,
      opeStrPriv
    )
  })
}

object PubKeys {
  implicit def codec: CodecJson[PubKeys] =
    casecodec2(PubKeys.apply,PubKeys.unapply)("paillier","elgamal")
}
