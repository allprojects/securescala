package crypto.dsl

import scalaz.std.list._

import org.scalatest._
import org.scalacheck.{Gen => SCGen}
import org.scalameter.api._

import crypto._
import crypto.cipher._

class AnalysisSpec extends WordSpec with Matchers {
  val keys = KeyRing.create
  val N = 300

  "Program Analysis" can {
    "count the number of required conversions" in {
      import Analysis._

      val additive = Common.encrypt(Additive, keys)(1)
      val multiplicative = Common.encrypt(Multiplicative, keys)(2)

      requiredConversions{ add(additive,additive)             } should equal(0)
      requiredConversions{ add(additive,multiplicative)       } should equal(1)
      requiredConversions{ add(multiplicative,additive)       } should equal(1)
      requiredConversions{ add(multiplicative,multiplicative) } should equal(2)
    }

    "extract the numbers from a program" in {
      import Analysis._
      val rand = new util.Random

      val data = List.fill(N)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))

      val ns: List[(Option[Scheme],EncInt)] = extractNumbers(sumOpt(data))

      ns.map(_._2) should equal(data)
    }

    "replace the numbers from a program" in {
      import Analysis._
      val rand = new util.Random

      val data1 = List.fill(N)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))
      val data2 = List.fill(N)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))

      val program: Crypto[Option[PaillierEnc]] = sumOpt(data1)
      val program2: Crypto[Option[PaillierEnc]] = replaceNumbers(program).eval(data2)

      extractNumbers(program2).map(_._2) should equal(data2)
    }

    "modify the numbers in a program" in {
      import Analysis._
      val rand = new util.Random

      val data = List.fill(N)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))

      val program = sumOpt(data)

      val program2 = withNumbers(program)(_.map(_._2).reverse)

      val ns: List[(Option[Scheme],EncInt)] = extractNumbers(program2)

      ns.map(_._2) should equal(data.reverse)
    }
  }
}

object AnalysisBench extends CustomPerformanceTest {
  val keyRing = KeyRing.create
  @transient val generators = EncryptedGens(keyRing)

  val sizes = Gen.enumeration("size")(50,100,150,200,250)
  val programs =
    for (size <- sizes)
    yield sumOpt(SCGen.listOfN(size, generators.encryptedNumber).sample.get)

  performance of "Analysis" in {
    measure method "requiredConversions" in {
      using(programs) in { p => Analysis.requiredConversions(p) }
    }

    measure method "extractConversions" in {
      using(programs) in { p => Analysis.extractConversions(p) }
    }
  }
}
