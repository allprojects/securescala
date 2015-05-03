package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

import crypto.TestUtils._

object OpeCheck extends Properties("OPE") {
  val (encrypt,decrypt) = Ope.createNum(128)

  property("decrypt · encrypt = id (Int)") =
    forAll(arbitrary[Int].retryUntil(_>=0)) { (input: Int) =>
      decrypt(encrypt(input)) == input
    }

}
