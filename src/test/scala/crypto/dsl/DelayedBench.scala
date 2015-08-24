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

  val delay = 150.milliseconds

  val ecLocal = CustomExecutionContext(4)
  val ecRemote = CustomExecutionContext(4)

  val cryptoService = new DelayedCryptoService(keyRing, delay)(ecRemote)

  val noOpt = new RemoteInterpreter(cryptoService,keyRing.pub)(ecLocal)

  val opt = new RemoteInterpreterOpt(cryptoService,keyRing.pub)(ecLocal)

  val optAnalyze =
    new RemoteInterpreterOptAnalyze(cryptoService,keyRing.pub,FixedBatch(15),_ >= 15)(ecLocal)

  val generators = EncryptedGens(keyRing)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val sizes = Gen.enumeration("size")(1,5,10,15,20,25,30,35,40)
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

  performance of s"Product (${delay})" in {
    measure method "sequential" in {
      using(lists) in { xs =>
        Await.result(noOpt.interpret(productA(one)(xs)), Duration.Inf)
      }
    }

    measure method "parallel" in {
      using(lists) in { xs =>
        Await.result(opt.interpret(productA(one)(xs)), Duration.Inf)
      }
    }

    measure method "parallel + batch" in {
      using(lists) in { xs =>
        Await.result(optAnalyze.interpret(productA(one)(xs)), Duration.Inf)
      }
    }
  }

  performance of s"Sorting (${delay})" in {
    measure method "sequential" in {
      using(lists) in { xs =>
        Await.result(noOpt.interpret(sorted(xs)), Duration.Inf)
      }
    }

    measure method "parallel" in {
      using(lists) in { xs =>
        Await.result(opt.interpret(sorted(xs)), Duration.Inf)
      }
    }

    measure method "parallel + batch" in {
      using(lists) in { xs =>
        Await.result(optAnalyze.interpret(sorted(xs)), Duration.Inf)
      }
    }
  }
}


// Test to see how the versions perform using a quick run
object DelayedBenchTestApp extends App {

  val keyRing = KeyRing.create

  List(50,100,150,200).map(_.milliseconds).foreach { delay =>
    println(s"**********\nDelay: ${delay}\n**********")

    val ecRemote = CustomExecutionContext(4)
    val ec = CustomExecutionContext(4)

    val cryptoService = new DelayedCryptoService(keyRing, delay)(ecRemote)

    val noOpt = new RemoteInterpreter(cryptoService,keyRing.pub)(ec)

    val opt = new RemoteInterpreterOpt(cryptoService,keyRing.pub)(ec)

    val optAnalyze =
      new RemoteInterpreterOptAnalyze(cryptoService,keyRing.pub,FixedBatch(15),_ > 10)(ec)

    val zero = Common.zero(keyRing)

    val data = SampleData.fixed1.map(Common.encrypt(Comparable, keyRing))
    List(1,5,10,15,20,25,30,35).map(data.take(_)).foreach { xs =>
      println(s"----- ${xs.size} -----")

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
  }
}

object CustomExecutionContext {
  def apply(threads: Int): ExecutionContext = ExecutionContext.fromExecutorService(
    new ForkJoinPool(threads, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true))
}
