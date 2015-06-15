package crypto.dsl

import argonaut._
import Argonaut._

import org.scalatest._

import crypto._
import crypto.cipher._

class EncSpec extends WordSpec with Matchers {
  val keys = KeyRing.create
  implicit val paillierDecode =
    PaillierEnc.decode(keys.pub.paillier)

  "Paillier encoded values" can {
    "be printed as json" in {
      val fortyTwo: PaillierEnc = Common.depEncrypt(Additive, keys)(42)
      fortyTwo.asJson.nospaces should equal(s"""{"paillier":${fortyTwo.underlying}}""")
    }

    "be parsed from json" in {
      val fortyTwo: PaillierEnc = Common.depEncrypt(Additive, keys)(42)
      val json = s"""{"paillier":${fortyTwo.underlying}}"""
      val dec = Parse.decodeOption[PaillierEnc](json)
      dec should equal(Some(fortyTwo))
    }
  }

  "ElGamal encoded values" can {
    "be printed as json" in {
    }
  }
}
