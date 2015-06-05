package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scalaz._

import crypto._

object ElGamalCheck extends Properties("ElGamal") with CryptoCheck {
  val (encrypt,decrypt,pub) = ElGamal.create(1024)

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
      encrypt(input).map(decrypt.apply) == \/-(input)
  }

  property("multiplicative homomorphic") =
    forAll(generators.allowedNumber, generators.allowedNumber) { (a: BigInt, b: BigInt) =>
    val \/-((ca1,ca2)) = encrypt(a)
    val \/-((cb1,cb2)) = encrypt(b)

    decrypt(ca1 * cb1, ca2 * cb2) == (a * b)
  }
}
