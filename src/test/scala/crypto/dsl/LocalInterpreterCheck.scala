package crypto.dsl

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import crypto._
import crypto.cipher._
import crypto.TestUtils._

import scalaz.syntax.traverse._
import scalaz.std.list._

object LocalInterpreterCheck extends Properties("LocalInterpreter") {
  val keyRing = KeyRing.create
  val locally = LocalInterpreter(keyRing)

  property("sum of a list") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val decryptThenSum = xs.map(Common.decrypt(keyRing.priv)).sum

      val sumThenDecrypt = Common.decrypt(keyRing.priv) {
        locally.interpret {
          xs.traverse(toPaillier).map(_.reduce(_+_)).monadic
        }
      }

      decryptThenSum == sumThenDecrypt
    }

  property("product of a list") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val decryptThenProd = xs.map(Common.decrypt(keyRing.priv)).product

      val prodThenDecrypt = Common.decrypt(keyRing.priv) {
        locally.interpret {
          xs.traverse(toGamal).map(_.reduce(_*_)).monadic
        }
      }

      decryptThenProd == prodThenDecrypt
    }

  property("monadic sum == applicative sum") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val zero@PaillierEnc(_) = Common.encrypt(Additive, keyRing.pub)(0)

      val monadicSum = locally.interpret { sumM(zero)(xs) }

      val applicativeSum = locally.interpret { sumA(zero)(xs).monadic }

      Common.decrypt(keyRing.priv)(monadicSum) == Common.decrypt(keyRing.priv)(applicativeSum)
    }

}
