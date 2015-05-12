package crypto.dsl

import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.concurrent._
import scala.concurrent.duration._

import crypto._
import crypto.remote._
import crypto.cipher._
import crypto.TestUtils._

import scalaz.syntax.traverse._
import scalaz.std.list._

trait InterpreterCheck[F[_]] { this: Properties =>
  val keyRing = KeyRing.create
  val interpreter: CryptoInterpreter[F]
  def finalize[A](x: F[A]): A

  def interpret[A](p: CryptoM[A]): A = finalize { interpreter.interpret(p) }

    property("sum of a list") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val decryptThenSum = xs.map(Common.decrypt(keyRing.priv)).sum

      val sumThenDecrypt = Common.decrypt(keyRing.priv) {
        interpret {
          xs.traverse(toPaillier).map(_.reduce(_+_)).monadic
        }
      }

      decryptThenSum == sumThenDecrypt
    }

  property("product of a list") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val decryptThenProd = xs.map(Common.decrypt(keyRing.priv)).product

      val prodThenDecrypt = Common.decrypt(keyRing.priv) {
        interpret {
          xs.traverse(toGamal).map(_.reduce(_*_)).monadic
        }
      }

      decryptThenProd == prodThenDecrypt
    }

  property("monadic sum == applicative sum") =
    forAll(nonEmptyEncryptedList(5)(keyRing)) { (xs: List[Enc]) =>
      val zero@PaillierEnc(_) = Common.encryptPub(Additive, keyRing.pub)(0)

      val monadicSum = interpret { sumM(zero)(xs) }

      val applicativeSum = interpret { sumA(zero)(xs).monadic }

      Common.decrypt(keyRing.priv)(monadicSum) == Common.decrypt(keyRing.priv)(applicativeSum)
    }

  property("encrypted sorting") = {
    // Integers can not be larger than bit size of ope key
    val list = for {
      n <- Gen.choose(1,6)
      xs <- Gen.listOfN(n, encryptedNumber(keyRing)(opeAllowedInts(keyRing.priv.opePriv)))
    } yield xs

    forAll(list) { (xs: List[Enc]) =>
      val decrypt = Common.decrypt(keyRing.priv)
      val encSort = interpret { sorted(xs) }.map(decrypt)
      val decSort = xs.map(decrypt).sorted
      encSort == decSort
    }
  }
}

object LocalInterpreterCheck
    extends Properties("LocalInterpreter")
    with InterpreterCheck[λ[α=>α]] {

  override val interpreter = LocalInterpreter(keyRing)
  override def finalize[A](x: A) = x
}

object RemoteInterpreterCheck
    extends Properties("RemoteInterpreter")
    with InterpreterCheck[Future] {

  val cryptoService = new CryptoServiceImpl(keyRing)
  override val interpreter = RemoteInterpreter(cryptoService)(
    scala.concurrent.ExecutionContext.Implicits.global)
  override def finalize[A](x: Future[A]) = Await.result(x, Duration.Inf)
}
