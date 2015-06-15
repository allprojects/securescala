package crypto.dsl

import argonaut._
import Argonaut._

import org.scalatest._

import crypto._
import crypto.cipher._

class EncSpec extends WordSpec with Matchers {
  val keys = KeyRing.create
  implicit val paillierDecode = PaillierEnc.decode(keys.pub.paillier)
  implicit val elGamalDecode = ElGamalEnc.decode(keys.pub.elgamal)

  "Paillier encoded values" can {
    val fortyTwo: PaillierEnc = Common.depEncrypt(Additive, keys)(42)

    "be printed as json" in {
      fortyTwo.asJson.nospaces should equal(s"""{"paillier":${fortyTwo.underlying}}""")
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[PaillierEnc](s"""{"paillier":${fortyTwo.underlying}}""")
      dec should equal(Some(fortyTwo))
    }
  }

  "ElGamal encoded values" can {
    val eightyNine: ElGamalEnc = Common.depEncrypt(Multiplicative, keys)(89)

    "be printed as json" in {
      eightyNine.asJson.nospaces should equal(
        s"""{"elgamal_ca":${eightyNine.ca},"elgamal_cb":${eightyNine.cb}}""")
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[ElGamalEnc](
        s"""{"elgamal_ca":${eightyNine.ca},"elgamal_cb":${eightyNine.cb}}""")
      dec should equal(Some(eightyNine))
    }
  }

  "Aes encoded numbers" can {
    val oneThreeThreeSeven: AesEnc = Common.depEncrypt(Equality, keys)(1337)
    val json = {
      val expected = oneThreeThreeSeven.underlying.toList.map(_.toInt).asJson.nospaces
      s"""{"aes_int":${expected}}"""
    }

    "be printed as json" in {
      oneThreeThreeSeven.asJson.nospaces should equal(json)
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[AesEnc](json)
      dec should equal(Some(oneThreeThreeSeven))
    }
  }
}
