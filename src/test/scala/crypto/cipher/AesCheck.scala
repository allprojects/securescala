package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

object AesCheck extends Properties("AES") {
  val (encrypt,decrypt,_) = Aes.create(Aes.B256)

  property("decrypt · encrypt = id (BigInt)") =
    forAll { (input: BigInt) =>
      BigInt(decrypt(encrypt(input.toByteArray))) == input
    }

  property("deterministic (BigInt)") = forAll { (a: BigInt) =>
    val c1 = encrypt(a.toByteArray)
    val c2 = encrypt(a.toByteArray)

    (c1.size == c2.size) :| "same number of bytes" &&
    ((c1,c2).zipped.forall(_==_)) :| "all bytes equal"
  }

  property("deterministic (String)") = forAll { (s: String) =>
    val c1 = encrypt(s.toCharArray.map(_.toByte))
    val c2 = encrypt(s.toCharArray.map(_.toByte))

    (c1.size == c2.size) :| "same number of bytes" &&
    ((c1,c2).zipped.forall(_==_)) :| "all bytes equal"
  }

}
