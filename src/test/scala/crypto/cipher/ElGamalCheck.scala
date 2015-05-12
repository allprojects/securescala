package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scalaz._

import crypto.TestUtils._

object ElGamalCheck extends Properties("ElGamal") {
  val (encrypt,decrypt,pub) = ElGamal.create(1024)

  property("decrypt · encrypt = id for positive ints") =
    forAll(posInt) { (input: BigInt) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (with modulus)") = forAll { (input: BigInt) =>
    encrypt(input).map(decrypt.apply) == \/-(input.mod(pub.p))
  }

  property("multiplicative homomorphic") = forAll { (a: BigInt, b: BigInt) =>
    val \/-((ca1,ca2)) = encrypt(a)
    val \/-((cb1,cb2)) = encrypt(b)

    decrypt(ca1 * cb1, ca2 * cb2).mod(pub.p) == (a * b).mod(pub.p)
  }

}
