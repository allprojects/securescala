package crypto.dsl

import scala.language.higherKinds

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.concurrent._
import scala.concurrent.duration._

import crypto._
import crypto.dsl.Implicits._
import crypto.remote._
import crypto.cipher._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.syntax.std.list._

trait InterpreterCheck[F[_]] extends CryptoCheck { this: Properties =>
  val interpreter: CryptoInterpreter[F]
  def finalize[A](x: F[A]): A

  def interpret[A](p: CryptoM[A]): A = finalize { interpreter.interpret(p) }

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  property("sum of a list") =
    forAll(generators.nonEmptyEncryptedList(10)) { (xs: List[EncInt]) =>
      val decryptThenSum = xs.map(Common.decrypt(keyRing.priv)).sum

      val sumThenDecrypt = Common.decrypt(keyRing.priv) {
        interpret {
          xs.traverse(toPaillier).map(_.reduce(_+_))
        }
      }

      decryptThenSum == sumThenDecrypt
    }

  property("product of a list") =
    forAll(generators.nonEmptyEncryptedList(10)) { (xs: List[EncInt]) =>
      val decryptThenProd = xs.map(Common.decrypt(keyRing.priv)).product

      val prodThenDecrypt = Common.decrypt(keyRing.priv) {
        interpret { productA(one)(xs) }
      }

      decryptThenProd == prodThenDecrypt
    }

  property("monadic sum == applicative sum") =
    forAll(generators.nonEmptyEncryptedList(10)) { (xs: List[EncInt]) =>

      val monadicSum = interpret { sumM(zero)(xs) }

      val applicativeSum = interpret { sumA(zero)(xs) }

      Common.decrypt(keyRing.priv)(monadicSum) == Common.decrypt(keyRing.priv)(applicativeSum)
    }

  property("monadic product == applicative product") =
    forAll(generators.nonEmptyEncryptedList(10)) { (xs: List[EncInt]) =>
      val monadicProduct = interpret {productM(one)(xs) }

      val applicativeProduct = interpret { productA(one)(xs) }

      Common.decrypt(keyRing.priv)(monadicProduct) == Common.decrypt(keyRing.priv)(applicativeProduct)
    }

  property("encrypted sorting of numbers") = {
    // Integers can not be larger than bit size of ope key
    val list = for {
      n <- Gen.choose(1,6)
      xs <- Gen.listOfN(n, generators.encryptedNumber)
    } yield xs

    forAll(list) { (xs: List[EncInt]) =>
      val decrypt = Common.decrypt(keyRing.priv)
      val encSort = interpret { sorted(xs) }.map(decrypt)
      val decSort = xs.map(decrypt).sorted
      encSort == decSort
    }
  }

  property("filterM vs filter") = {
    forAll(generators.nonEmptyEncryptedList(10)) { (xs: List[EncInt]) =>
      def predM(n: EncInt): Crypto[Boolean] = isEven(n)
      def pred(n: BigInt): Boolean = n.mod(2) == 0

      val filterThenDecrypt = interpret(xs.filterM(predM)).map(Common.decrypt(keyRing.priv))
      val decryptThenFilter = xs.map(Common.decrypt(keyRing.priv)).filter(pred)

      filterThenDecrypt == decryptThenFilter
    }
  }

  property("encrypted sorting of strings") = {
    def sortWords(input: List[EncString]) = input.traverse(toOpeStr(_)).map(_.sorted)

    forAll(Gen.listOf(generators.encryptedString)) { (xs: List[EncString]) =>
      val sortThenDecrypt = interpret(sortWords(xs)).map(Common.decryptStr(keyRing))
      val decryptThenSort = xs.map(Common.decryptStr(keyRing)).sorted

      sortThenDecrypt == decryptThenSort
    }
  }

  property("encrypted string concatenation") = {
    forAll(generators.encryptedString,generators.encryptedString) {
      (s1: EncString, s2: EncString) =>

      val concatThenDecrypt = Common.decryptStr(keyRing)(interpret(concatStr(s1,s2)))
      val decryptThenConcat =
        Common.decryptStr(keyRing)(s1) ++ Common.decryptStr(keyRing)(s1)

      concatThenDecrypt == decryptThenConcat
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

  val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  override val interpreter = RemoteInterpreter(cryptoService, pubKeys)(
    scala.concurrent.ExecutionContext.Implicits.global)
  override def finalize[A](x: Future[A]) = Await.result(x, Duration.Inf)
}

object RemoteInterpreterOptCheck
    extends Properties("RemoteInterpreterOpt")
    with InterpreterCheck[Future] {

  val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  override val interpreter = new RemoteInterpreterOpt(cryptoService, pubKeys)(
    scala.concurrent.ExecutionContext.Implicits.global)
  override def finalize[A](x: Future[A]) = Await.result(x, Duration.Inf)
}

object RemoteInterpreterOptAnalyzeCheck
    extends Properties("RemoteInterpreterOptAnalyze")
    with InterpreterCheck[Future] {

  val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))
  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)
  override val interpreter =
    new RemoteInterpreterOptAnalyze(cryptoService, pubKeys, FixedBatch(10), _ > 3)(
      scala.concurrent.ExecutionContext.Implicits.global)
  override def finalize[A](x: Future[A]) = Await.result(x, Duration.Inf)
}
