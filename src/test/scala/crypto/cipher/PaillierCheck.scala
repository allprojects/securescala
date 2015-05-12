package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scalaz._

import crypto.TestUtils._

object PaillierCheck extends Properties("Paillier") {
  val (encrypt,decrypt,pub) = Paillier.create(1024)

  property("decrypt · encrypt = id for positive ints") =
    forAll(posInt) { (input: BigInt) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (with modulus)") = forAll { (input: BigInt) =>
    encrypt(input).map(decrypt.apply) == \/-(input.mod(pub.n))
  }

  property("additive homomorphic") = forAll { (a: BigInt, b: BigInt) =>
    val \/-(ea) = encrypt(a)
    val \/-(eb) = encrypt(b)
    decrypt((ea * eb) mod pub.nSquare) == (a + b).mod(pub.n)
  }

}
