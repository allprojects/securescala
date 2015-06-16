package crypto

import argonaut._
import Argonaut._

import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

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

  "Ope encoded numbers" can {
    val ninetyNine: OpeEnc = Common.depEncrypt(Comparable, keys)(99)
    val json = s"""{"ope_int":${ninetyNine.underlying}}"""

    "be printed as json" in {
      ninetyNine.asJson.nospaces should equal(json)
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[OpeEnc](json)
      dec should equal(Some(ninetyNine))
    }
  }

  "Aes encoded strings" can {
    val helloWorld = Common.encryptStrAes(keys)("hello world")
    val json = {
      val expected = helloWorld.underlying.toList.map(_.toInt).asJson.nospaces
      s"""{"aes_str":${expected}}"""
    }

    "be printed as json" in {
      helloWorld.asJson.nospaces should equal(json)
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[AesString](json)
      dec should equal(Some(helloWorld))
    }
  }

  "Ope encoded strings" can {
    val scala: OpeString = Common.encryptStrOpe(keys)("scala")
    val json = s"""{"ope_str":${scala.underlying}}"""

    "be printed as json" in {
      scala.asJson.nospaces should equal(json)
    }

    "be parsed from json" in {
      val dec = Parse.decodeOption[OpeString](json)
      dec should equal(Some(scala))
    }
  }
}

object EncCheck extends Properties("Enc") with CryptoCheck {
  import generators._
  implicit val decoderInt = EncInt.decode(keyRing)

  property("fromJSON . toJSON = id (numbers)") =
    forAll(encryptedNumber) { (input: EncInt) =>
      Parse.decodeOption[EncInt](input.asJson.nospaces) == Some(input)
    }

  property("fromJSON . toJSON = id (strings)") =
    forAll(encryptedString) { (input: EncString) =>
      Parse.decodeOption[EncString](input.asJson.nospaces) == Some(input)
    }
}
