package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

import scalaz._
import scalaz.syntax.order._
import scalaz.std.math.bigInt._

import crypto._

object OpeCheck extends Properties("OPE") with CryptoCheck {
  val (encrypt,decrypt,key) =
    (keyRing.priv.opeIntEnc,keyRing.priv.opeIntDec,keyRing.priv.opePriv)

  property("decrypt · encrypt = id (Int)") =
    forAll(arbitrary[Int].retryUntil(_ >= 0) ){ (input: Int) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (limited BigInt)") =
    forAll(generators.allowedNumber) { (input: BigInt) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("preserves ordering") =
    forAll(generators.allowedNumber, generators.allowedNumber) { (a: BigInt, b: BigInt) =>
      val \/-(ea) = encrypt(a)
      val \/-(eb) = encrypt(b)
      a ?|? b == ea ?|? eb
    }

  property("generator produces valid numbers") =
    forAll(generators.encryptedNumber) { (x: Enc) =>
      val decrypted = Common.decrypt(keyRing.priv)(x)
      encrypt(decrypted).isRight
    }
}
