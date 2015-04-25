package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import TestUtils._

object PaillierCheck extends Properties("Paillier") {
  val (encrypt,decrypt,pub) = Paillier.create(1024)

  property("decrypt · encrypt = id for positive ints") =
    forAll(positiveInts) { (input: BigInt) =>
      decrypt(encrypt(input)) == input
    }

  property("decrypt · encrypt = id (with modulus)") = forAll { (input: BigInt) =>
    decrypt(encrypt(input)) == input.mod(pub.n)
  }

  property("additive homomorphic") = forAll { (a: BigInt, b: BigInt) =>
    decrypt((encrypt(a) * encrypt(b)) mod pub.nSquare) == (a + b).mod(pub.n)
  }

}
