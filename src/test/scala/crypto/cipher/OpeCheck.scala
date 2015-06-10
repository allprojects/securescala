package crypto.cipher

import scala.concurrent.duration._
import scala.concurrent._

import org.scalatest._

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

import scalaz._
import scalaz.syntax.order._
import scalaz.std.math.bigInt._
import scalaz.std.string._

import crypto._

object OpeCheck extends Properties("OPE") with CryptoCheck {
  val (encrypt,decrypt,key) =
    (keyRing.priv.opeIntEnc,keyRing.priv.opeIntDec,keyRing.priv.opeIntPriv)
  val (strEncrypt,strDecrypt,strKey) =
    (keyRing.priv.opeStrEnc,keyRing.priv.opeStrDec,keyRing.priv.opeStrPriv)

  property("generator produces valid numbers") =
    forAll(generators.allowedNumber) { (x: BigInt) =>
      encrypt(x).isRight
    }

  property("numericToPlain · plainToNumeric = id") =
    forAll(generators.allowedString) { (s: String) =>
      OpeStr.plainToNumeric(strKey)(s).flatMap(OpeStr.numericToPlain(strKey)(_)) == \/-(s)
    }

  property("decrypt · encrypt = id (Int)") =
    forAll { (input: Int) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (Long)") =
    forAll { (input: Long) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (limited BigInt)") =
    forAll(generators.allowedNumber) { (input: BigInt) =>
      encrypt(input).map(decrypt.apply) == \/-(input)
    }

  property("decrypt · encrypt = id (String)") =
    forAll(generators.allowedString) { (input: String) =>
      strEncrypt(input).flatMap(strDecrypt.apply) == \/-(input)
    }

  property("preserves ordering (numbers)") =
    forAll(generators.allowedNumber, generators.allowedNumber) { (a: BigInt, b: BigInt) =>
      val \/-(ea) = encrypt(a)
      val \/-(eb) = encrypt(b)
      a ?|? b == ea ?|? eb
    }

  property("preserves ordering (strings)") =
    forAll(generators.allowedString, generators.allowedString) { (a: String, b: String) =>
      val \/-(ea) = strEncrypt(a)
      val \/-(eb) = strEncrypt(b)
      a ?|? b == ea ?|? eb
    }
}

class OpeSpec extends WordSpec with Matchers {
  val keys = KeyRing.create

  "Ope encryption" can {
    "be used in a thread safe way" in {
      import scala.concurrent.ExecutionContext.Implicits.global
      val list: List[BigInt] = (1 to 100).map(BigInt(_)).toList

      val result = Future.sequence(
        list.map(x => Future(Common.encrypt(Comparable, keys)(x)))).
        map(_.map(Common.decrypt(keys.priv)))

      Await.result(result, Duration.Inf) should equal(list)
    }
  }
}
