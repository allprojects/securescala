package crypto.dsl

import org.scalatest._

import crypto._
import crypto.cipher._

class AnalysisSpec extends WordSpec with Matchers {
  val keys = KeyRing.create

  "Program Analysis" can {
    "count the number of required conversions" in {
      import Analysis._

      val additive = Common.encrypt(Additive, keys.pub)(1)
      val multiplicative = Common.encrypt(Multiplicative, keys.pub)(2)

      requiredConversions{ add(additive,additive)             } should equal(0)
      requiredConversions{ add(additive,multiplicative)       } should equal(1)
      requiredConversions{ add(multiplicative,additive)       } should equal(1)
      requiredConversions{ add(multiplicative,multiplicative) } should equal(2)
    }
    "reduce the number of conversions" in {
    }
  }
}
