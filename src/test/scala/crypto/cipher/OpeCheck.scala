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

import TestUtils._

object OpeCheck extends Properties("OPE") {
  val (encrypt,decrypt,key) = Ope.createNum(128)


  property("decrypt · encrypt = id (Int)") =
    forAll(arbitrary[Int].retryUntil(_ >= 0) ){ (input: Int) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (limited BigInt)") =
    forAll(opeAllowedInts(key)) { (input: BigInt) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("preserves ordering") =
    forAll(opeAllowedInts(key),opeAllowedInts(key)) { (a: BigInt, b: BigInt) =>
      val \/-(ea) = encrypt(a)
      val \/-(eb) = encrypt(b)
      a ?|? b == ea ?|? eb
  }
}
