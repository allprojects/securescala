package crypto.cipher

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

import scalaz.syntax.order._
import scalaz.std.math.bigInt._

object OpeCheck extends Properties("OPE") {
  val (encrypt,decrypt,key) = Ope.createNum(128)

  val allowedBigInts = arbitrary[BigInt] retryUntil (x => x >=0 && x.bitLength <= key.plainBits)

  property("decrypt · encrypt = id (Int)") =
    forAll(arbitrary[Int].retryUntil(_ >= 0) ){ (input: Int) =>
      decrypt(encrypt(input)) == input
    }

  property("decrypt · encrypt = id (limited BigInt)") =
    forAll(allowedBigInts) { (input: BigInt) =>
      decrypt(encrypt(input)) == input
    }

  property("preserves ordering") = forAll(allowedBigInts,allowedBigInts) { (a: BigInt, b: BigInt) =>
    a ?|? b == encrypt(a) ?|? encrypt(b)
  }

}
