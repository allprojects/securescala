package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.duration._

import scalaz.std.list._

import org.scalacheck.{Gen => SCGen}

import crypto._
import crypto.cipher._
import crypto.remote._

import org.scalameter.api._

import scala.concurrent.ExecutionContext.Implicits.global

trait InterpreterBench[F[_]] extends CryptoCheck {
  this: PerformanceTest =>

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val sizes = Gen.range("size")(10,50,20)
  val lists = for (size <- sizes) yield SCGen.listOfN(size, generators.encryptedNumber).sample.get

  // To be implemented
  def finalize[A]: F[A] => A
  def interpret[A]: CryptoM[A] => F[A]

  // The specific test cases
  performance of "Sum" in {
    measure method "monadic" in {
      using(lists) in { xs => finalize { interpret(sumM(zero)(xs)) } }
    }

    measure method "applicative" in {
      using(lists) in { xs => finalize { interpret(sumA(zero)(xs)) } }
    }
  }

  performance of "Product" in {
    measure method "monadic" in {
      using(lists) in { xs => finalize { interpret(productM(one)(xs)) } }
    }

    measure method "applicative" in {
      using(lists) in { xs => finalize { interpret(productA(one)(xs)) } }
    }
  }

  performance of "Sort" in {
    measure method "applicative" in {
      using(lists) in { xs => finalize { interpret(sorted(xs)) } }
    }
  }
}

object RemoteInterpreterBench
    extends PerformanceTest.Quickbenchmark
    with InterpreterBench[Future] {

  val cryptoService = new CryptoServiceImpl(keyRing)
  val interpreter = new RemoteInterpreter(cryptoService)

  override def interpret[A] = (x: CryptoM[A]) => interpreter.interpret(x)
  override def finalize[A] = (x: Future[A]) => Await.result(x,Duration.Inf)
}

object LocalInterpreterBench
    extends PerformanceTest.Quickbenchmark
    with InterpreterBench[λ[α=>α]] {

  val interpreter = LocalInterpreter(keyRing)

  override def interpret[A] = (x: CryptoM[A]) => interpreter.interpret(x)
  override def finalize[A] = (x: A) => x
}
