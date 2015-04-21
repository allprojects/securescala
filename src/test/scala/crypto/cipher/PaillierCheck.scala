package crypto.cipher

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object PaillierCheck extends Properties("Paillier") {
  val (encrypt,decrypt,pub) = Paillier.create(512)

  property("decrypt . encrypt = id") = forAll { (input: BigInt) =>
    decrypt(encrypt(input)) == input.mod(pub.n)
  }

  property("additive homomorphic") = forAll { (a: BigInt, b: BigInt) =>
    decrypt((encrypt(a) * encrypt(b)) mod pub.nSquare) == (a + b).mod(pub.n)
  }
}
