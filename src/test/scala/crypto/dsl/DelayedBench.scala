package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.forkjoin._
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

  val delay = 100.milliseconds

  @transient val cryptoService = new DelayedCryptoService(keyRing, delay)

  val ec = ExecutionContext.fromExecutorService(new ForkJoinPool(10, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true))

  @transient val noOpt = new RemoteInterpreter(cryptoService,keyRing.pub)(ec)

  @transient val opt = new RemoteInterpreterOpt(cryptoService,keyRing.pub)(ec)

  // Start batching if more than 3 conversions
  @transient val optAnalyze =
    new RemoteInterpreterOptAnalyze(cryptoService,keyRing.pub,FixedBatch(10),_ > 5)(ec)

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


// Test to see how the versions perform using a quick run
object DelayedBenchTestApp extends App {

  val keyRing = KeyRing.create

  val delay = 1000.milliseconds

  val cryptoService = new DelayedCryptoService(keyRing, delay)

  implicit val ec = ExecutionContext.Implicits.global

  val noOpt = new RemoteInterpreter(cryptoService,keyRing.pub)

  val opt = new RemoteInterpreterOpt(cryptoService,keyRing.pub)

  val optAnalyze =
    new RemoteInterpreterOptAnalyze(cryptoService,keyRing.pub,FixedBatch(10),_ >= 5)

  val generators = EncryptedGens(keyRing)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val xs = SampleData.fixed1.map(Common.encrypt(Comparable, keyRing)).take(15)

  var start = System.currentTimeMillis
  Await.result(noOpt.interpret(sumA(zero)(xs)), Duration.Inf)
  println(s"noopt: ${System.currentTimeMillis - start}")
  start = System.currentTimeMillis
  Await.result(opt.interpret(sumA(zero)(xs)), Duration.Inf)
  println(s"opt: ${System.currentTimeMillis - start}")
  start = System.currentTimeMillis
  Await.result(optAnalyze.interpret(sumA(zero)(xs)), Duration.Inf)
  println(s"analyze: ${System.currentTimeMillis - start}")
}
