package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import org.scalacheck.{Gen => SCGen}

import org.scalameter.api._

import crypto._
import crypto.dsl.Implicits._
import crypto.cipher._
import crypto.remote._

import scalaz.std.list._

object DelayedBench extends CustomPerformanceTest {

  val keyRing = KeyRing.create

  val delay = 300.milliseconds

  @transient val cryptoService = new DelayedCryptoService(keyRing, delay)

  implicit val ec = ExecutionContext.Implicits.global

  @transient val noOpt = new RemoteInterpreter(cryptoService,keyRing.pub)

  @transient val opt = new RemoteInterpreterOpt(cryptoService,keyRing.pub)

  // Start batching if more than 3 conversions
  @transient val optAnalyze = new RemoteInterpreterOptAnalyze(cryptoService,keyRing.pub,5)

  @transient val generators = EncryptedGens(keyRing)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val sizes = Gen.enumeration("size")(1,5,10,15,20)
  val lists =
    for (size <- sizes) yield SCGen.listOfN(size, generators.encryptedNumber).sample.get

  performance of s"Sum (${delay})" in {
    measure method "sequential" in {
      using(lists) in { xs =>
        Await.result(noOpt.interpret(sumA(zero)(xs)), Duration.Inf)
      }
    }

    measure method "parallel" in {
      using(lists) in { xs =>
        Await.result(opt.interpret(sumA(zero)(xs)), Duration.Inf)
      }
    }

    measure method "parallel + batch" in {
      using(lists) in { xs =>
        Await.result(optAnalyze.interpret(sumA(zero)(xs)), Duration.Inf)
      }
    }

  }
}
