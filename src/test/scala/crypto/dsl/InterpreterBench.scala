package crypto.dsl

import scala.language.higherKinds

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

import scalaz.std.list._

import org.scalacheck.{Gen => SCGen}

import org.scalameter.api._

import crypto._
import crypto.cipher._
import crypto.remote._

trait InterpreterBench[F[_]] {
  this: PerformanceTest =>

  val keyRing = KeyRing.create
  def generators = EncryptedGens(keyRing)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)

  val sizes = Gen.enumeration("size")(2,4,6,8,10)
  val lists =
    for (size <- sizes) yield SCGen.listOfN(size, generators.encryptedNumber).sample.get

  // To be implemented
  def finalize[A]: F[A] => A
  def createInterpreter: CryptoInterpreter[F]
  def name: String

  // The specific test cases
  performance of name in {
    lazy val interp = createInterpreter

    measure method "monadic sum" in {
      using(lists) in { xs => finalize { interp.interpret(sumM(zero)(xs)) } }
    }

    measure method "applicative sum" in {
      using(lists) in { xs => finalize { interp.interpretA(sumA(zero)(xs)) } }
    }

    measure method "monadic product" in {
      using(lists) in { xs => finalize { interp.interpret(productM(one)(xs)) } }
    }

    measure method "applicative product" in {
      using(lists) in { xs => finalize { interp.interpretA(productA(one)(xs)) } }
    }

    measure method "sorting" in {
      using(lists) in { xs => finalize { interp.interpretA(sorted(xs)) } }
    }
  }
}

class RemoteInterpreterOptAnalyzeBench
    extends CustomPerformanceTest
    with InterpreterBench[Future] {

  def name = "Remote interpreter with opt + analysis"
  def cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))

  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)

  def createInterpreter =
    new RemoteInterpreterOptAnalyze(cryptoService, pubKeys, FixedBatch(20), _ >= 10)(
      ExecutionContext.Implicits.global)

  override def finalize[A] = (x: Future[A]) => Await.result(x,Duration.Inf)
}

class RemoteInterpreterOptBench
    extends CustomPerformanceTest
    with InterpreterBench[Future] {

  def name = "Remote interpreter with opt"
  def cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))

  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)

  def createInterpreter =
    new RemoteInterpreterOpt(cryptoService, pubKeys)(ExecutionContext.Implicits.global)

  override def finalize[A] = (x: Future[A]) => Await.result(x,Duration.Inf)
}

class RemoteInterpreterBench
    extends CustomPerformanceTest
    with InterpreterBench[Future] {

  def name = "Remote interpreter"
  def cryptoService = new CryptoServiceImpl(keyRing)(CustomExecutionContext(5))

  val pubKeys = Await.result(cryptoService.publicKeys, 10.seconds)

  def createInterpreter =
    new RemoteInterpreter(cryptoService, pubKeys)(ExecutionContext.Implicits.global)

  override def finalize[A] = (x: Future[A]) => Await.result(x,Duration.Inf)
}

class LocalInterpreterBench
    extends CustomPerformanceTest
    with InterpreterBench[λ[α=>α]] {

  def name = "Local interpreter"
  def createInterpreter = LocalInterpreter(keyRing)

  override def finalize[A] = (x: A) => x
}

class InterpreterBenchSuite extends CustomPerformanceTest {
  // include[LocalInterpreterBench]
  include[RemoteInterpreterBench]
  include[RemoteInterpreterOptBench]
  include[RemoteInterpreterOptAnalyzeBench]
}

object InterpreterBenchSuiteRunner extends App {
  val bench = new InterpreterBenchSuite
  bench.main(args)
}
