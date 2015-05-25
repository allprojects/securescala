package crypto.dsl

import scalaz.std.list._

import org.scalatest._

import crypto._
import crypto.cipher._

class AnalysisSpec extends WordSpec with Matchers {
  val keys = KeyRing.create

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

      val data = List.fill(500)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))

      val ns: List[(Option[Scheme],Enc)] = extractNumbers(sumOpt(data))

      ns.map(_._2) should equal(data)
    }

    "replace the numbers from a program" in {
      import Analysis._
      val rand = new util.Random

      val data1 = List.fill(300)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))
      val data2 = List.fill(300)(Common.encrypt(Additive, keys)(BigInt(rand.nextInt.abs)))

      val program: Crypto[Option[PaillierEnc]] = sumOpt(data1)
      val program2: Crypto[Option[PaillierEnc]] = replaceNumbers(program).eval(data2)

      extractNumbers(program2).map(_._2) should equal(data2)
    }
  }
}
