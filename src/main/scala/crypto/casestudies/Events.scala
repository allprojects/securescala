package crypto.casestudies

import com.espertech.esper.client.Configuration
import com.espertech.esper.client.Configuration
import com.espertech.esper.client.ConfigurationPlugInAggregationFunction
import com.espertech.esper.client.EPAdministrator
import com.espertech.esper.client.EPRuntime
import com.espertech.esper.client.EPServiceProvider
import com.espertech.esper.client.EPServiceProviderManager
import com.espertech.esper.client.EPStatement
import com.espertech.esper.client.EventBean
import com.espertech.esper.client.UpdateListener
import com.espertech.esper.client.hook.AggregationFunctionFactory
import com.espertech.esper.client.time.CurrentTimeEvent
import com.espertech.esper.epl.agg.aggregator.AggregationMethod
import com.espertech.esper.epl.agg.service.AggregationValidationContext

import scalaz.std.function._
import scalaz.syntax.profunctor._

import scala.beans.BeanProperty
import scala.util._

import crypto._
import crypto.cipher._
import crypto.dsl.Implicits._
import crypto.dsl._

case class CryptoEvent(
  @BeanProperty name: String,
  @BeanProperty plainValue: Int,
  @BeanProperty encValue: EncInt
) {
  // needs 'get' prefix due to bean style used by esper
  def getPretty: String = s"""CryptoEvent("${name}",${plainValue},<encrypted>)"""
}

trait EsperImplicits {
  implicit class RichEPStatement(s: EPStatement) {
    def +=(f: Seq[EventBean] => Unit) = s.addListener(new UpdateListener {
      def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]): Unit = {
        f(newEvents)
      }
    })
  }
}

object EsperFilters extends App with EsperImplicits {
  val config: Configuration = new Configuration
  config.addImport("crypto.casestudies.*")
  config.addEventType(classOf[CryptoEvent])
  val functionName = "encSum"
  config.addPlugInAggregationFunctionFactory(functionName, classOf[TheInterpreter.EncryptedSumFactory].getName)
  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider(config)

  val evenNumbers: String = """
SELECT *
FROM CryptoEvent as cevt
WHERE TheInterpreter.isEven(cevt.encValue)
"""

  val smaller100: String = """
SELECT *
FROM CryptoEvent as cevt
WHERE TheInterpreter.smaller100(cevt.encValue)
"""

  val encryptedSum: String = """
SELECT encSum(cevt.encValue) as encValue
FROM CryptoEvent as cevt
"""

  epService.getEPAdministrator.createEPL(evenNumbers) += ((e: EventBean) =>
    println(f"${e.get("plainValue")}%3s is EVEN")).mapfst(x => x.head)

  epService.getEPAdministrator.createEPL(smaller100) += ((e: EventBean) =>
    println(f"${e.get("plainValue")}%3s is <100")).mapfst(x => x.head)

  epService.getEPAdministrator.createEPL(encryptedSum) += { (e: EventBean) =>
    val encrypted = e.get("encValue").asInstanceOf[EncInt]
    val decrypted = Common.decrypt(TheInterpreter.keyRing.priv)(encrypted)
    println(s"--- Current Sum: ${decrypted}")}.mapfst(x => x.head)

  val rand = new Random
  (1 to 100) foreach { n =>
    val randomInt = rand.nextInt(500)
    epService.getEPRuntime.sendEvent(
      CryptoEvent(s"Event($n)", randomInt, TheInterpreter.encrypt(randomInt)))
    Thread.sleep(100)
  }
}

object TheInterpreter {
  val keyRing = KeyRing.create
  val interp = LocalInterpreter(keyRing)
  //----------
  class EncryptedSumFactory extends AggregationFunctionFactory {
    def setFunctionName(functionName: String): Unit = ()
    def validate(validationContext: AggregationValidationContext): Unit =
      if ( (validationContext.getParameterTypes.length != 1) ||
        (validationContext.getParameterTypes.head != classOf[EncInt])) {
        throw new IllegalArgumentException("Aggregation requires a single parameter of type EncInt")
      }
    def getValueType(): Class[_] = classOf[EncInt]
    def newAggregator(): AggregationMethod =
      new EncryptedSumAggregationFunction
  }

  class EncryptedSumAggregationFunction extends AggregationMethod {
    val zero = Common.zero(keyRing)
    private var sumValue: EncInt = zero

    def clear(): Unit = sumValue = zero
    def getValueType(): Class[_] = classOf[EncInt]
    def enter(x_ : Any): Unit = {
      val x = x_.asInstanceOf[EncInt]
      sumValue = interp(sumValue + x)
    }
    def leave(x_ : Any): Unit = {
      val x = x_.asInstanceOf[EncInt]
      sumValue = interp(sumValue - x)
    }
    def getValue(): Object = sumValue
  }

  //----------
  
  def encrypt(i: BigInt) = Common.encryptPub(Additive, keyRing)(i).valueOr(sys.error)

  def isEven(e: EncInt): Boolean = interp(dsl.isEven(e))

  val onehundred = Common.encrypt(Comparable, keyRing)(100)
  def smaller100(e: EncInt): Boolean =interp(e < onehundred)
}

object EncryptedSumAggregationFunction {
  def register(keyRing: KeyRing, interp: PureCryptoInterpreter)(cfg: Configuration) = {

  }
}

