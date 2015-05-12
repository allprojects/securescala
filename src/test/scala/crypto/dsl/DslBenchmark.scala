package crypto.dsl

import scala.concurrent._
import scala.concurrent.duration._

import scalaz.std.list._

import org.scalacheck.{Gen => SCGen}

import crypto._
import crypto.cipher._
import crypto.TestUtils._

import org.scalameter.api._

import scala.concurrent.ExecutionContext.Implicits.global

object DslBenchmark extends PerformanceTest.Quickbenchmark {
  val keyRing = KeyRing.create
  val locally = LocalInterpreter(keyRing)
  val zero@PaillierEnc(_) = Common.encrypt(Additive, keyRing)(0)

  val sizes = Gen.range("size")(10,50,20)
  val lists = for (size <- sizes) yield SCGen.listOfN(size, encryptedNumber(keyRing)(TestUtils.posInt)).sample.get

  performance of "Sum" in {
    measure method "monadic" in {
      using(lists) in { xs =>
        locally.interpret(sumM(zero)(xs))
      }
    }

    measure method "applicative" in {
      using(lists) in { xs =>
        locally.interpret(sumA(zero)(xs))
      }
    }
  }
}
