package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scalaz._

import crypto._

object PaillierCheck extends Properties("Paillier") with CryptoCheck {
  val (encrypt,decrypt,pub) = Paillier.create(1024)

  property("decrypt · encrypt = id (Int)") =
    forAll { (input: Int) =>
      encrypt(input).map(decrypt(_)) == \/-(input)
    }

  property("decrypt · encrypt = id (Long)") =
    forAll { (input: Long) =>
      encrypt(input).map(decrypt(_)) == \/-(input)
    }

  property("decrypt · encrypt = id (limited BigInt)") =
    forAll(generators.allowedNumber) { (input: BigInt) =>
      encrypt(input).map(decrypt(_)) == \/-(input)
  }

  property("additive homomorphic") =
    forAll(generators.allowedNumber,generators.allowedNumber) { (a: BigInt, b: BigInt) =>
      val \/-(ea) = encrypt(a)
      val \/-(eb) = encrypt(b)
      decrypt(ea * eb) == (a + b)
  }
}
