package crypto

import argonaut._
import Argonaut._

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object KeyRingCheck extends Properties("KeyRing") with CryptoCheck {
  import generators._

  property("fromJSON . toJSON = id") =
    forAll(keyRingGen) { (keyRing: KeyRing) =>
      val asJson = keyRing.asJson.nospaces
      val keyRing_ = Parse.decodeOption[KeyRing](asJson)
      keyRing_.map(_.asJson.nospaces) == Some(asJson)
    }
}
