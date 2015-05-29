package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import org.scalacheck.{Gen => SCGen}

import org.scalameter.api._

import crypto._
import crypto.cipher._
import crypto.remote._

private object ScalaPrograms {
  def factorial(i: Int): Int = if (i == 0) 1 else i*factorial(i-1)
  def fib(i: Int): Int = if (i <= 1) 1 else fib(i-1) + fib(i-2)
  def collatzConjecture(i: Int): Int = if (! (i > 1)) i else {
    if (i % 2 == 0) collatzConjecture(i / 2) else collatzConjecture(3*i+1)
  }
}

object FactFibBench extends CustomPerformanceTest {

  val keyRing = KeyRing.create
   @transient val cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))

  @transient val remote =
    new RemoteInterpreterOpt(cryptoService,keyRing.pub)(ExecutionContext.Implicits.global)

  @transient val local = LocalInterpreter(keyRing)

  @transient val generators = EncryptedGens(keyRing)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val factNs = for {
    n <- Gen.range("n")(0,10,1)
  } yield (n, Common.encrypt(Comparable, keyRing)(n))

  val fibNs = for {
    n <- Gen.range("n")(0,5,1)
  } yield (n, Common.encrypt(Comparable, keyRing)(n))

  val collatzNs = for {
    n <- Gen.range("n")(1,9,2)
  } yield (n, Common.encrypt(Comparable, keyRing)(n))

  import ExamplePrograms.factorial
  import ExamplePrograms.fib
  import ExamplePrograms.collatzConjecture

  performance of "Factorial" in {
    measure method "plain scala" in {
      using(factNs) in { n => ScalaPrograms.factorial(n._1) }
    }

    measure method "local interpreter" in {
      using(factNs) in { n =>
        local.interpret(factorial(n._2))
      }
    }

    measure method "remote interpreter" in {
      using(factNs) in { n =>
        Await.result(remote.interpret(factorial(n._2)),Duration.Inf)
      }
    }
  }

  performance of "Fibonacci" in {
    measure method "plain scala" in {
      using(fibNs) in { n => ScalaPrograms.fib(n._1) }
    }

    measure method "local interpreter" in {
      using(fibNs) in { n =>
        local.interpret(fib(n._2))
      }
    }

    measure method "remote interpreter" in {
      using(fibNs) in { n =>
        Await.result(remote.interpret(fib(n._2)), Duration.Inf)
      }
    }
  }

  performance of "Collatz's Conjecture" in {
    measure method "local interpreter" in {
      using(collatzNs) in { n => local.interpret(collatzConjecture(n._2)) }
    }

    measure method "remote  interpreter" in {
      using(collatzNs) in { n =>
        Await.result(remote.interpret(collatzConjecture(n._2)),Duration.Inf)
      }
    }

    measure method "plain scala" in {
      using(collatzNs) in { n => ScalaPrograms.collatzConjecture(n._1) }
    }
  }
}
