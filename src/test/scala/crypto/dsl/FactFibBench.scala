package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import scalaz.Ordering._
import scalaz.syntax.applicative._

import org.scalacheck.{Gen => SCGen}

import org.scalameter.api._

import crypto._
import crypto.dsl.Implicits._
import crypto.cipher._
import crypto.remote._

private object Programs {
  def factorial(n: Enc): CryptoM[Enc] = for {
    zeroOne <- encrypt(Multiplicative)(0).tuple(encrypt(Multiplicative)(1))
    r <- factorialHelper(zeroOne._1,zeroOne._2)(n)
  } yield r

  def factorialHelper(zero: Enc, one: Enc)(n: Enc): CryptoM[Enc] = for {
    cond <- n =:= zero
    r <- if (cond) {
      one.point[CryptoM]
    } else for {
      n1 <- n - one
      fact <- factorialHelper(zero,one)(n1)
      s <- n * fact
    } yield s
  } yield r

  def fib(n: Enc): CryptoM[Enc] = for {
    one <- encrypt(Additive)(1)
    two <- encrypt(Additive)(2)
    r <- fibHelper(one,two)(n)
  } yield r

  def fibHelper(one: Enc, two: Enc)(n: Enc): CryptoM[Enc] = for {
    cmp <- n ?|? one
    r <- if (cmp == LT || cmp == EQ) {
      one.point[CryptoM]
    } else for {
      n12 <- (n-one).tuple(n-two)
      f1 <- fibHelper(one,two)(n12._1)
      f2 <- fibHelper(one,two)(n12._2)
      s <- f1 + f2
    } yield s
  } yield r

}

object FactFibBench extends CustomPerformanceTest {

  val keyRing = KeyRing.create
   @transient val cryptoService = new CryptoServiceImpl(keyRing)

  @transient val remote =
    new RemoteInterpreter(cryptoService)(ExecutionContext.Implicits.global)

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

  import Programs._

  performance of "Factorial" in {
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
}
