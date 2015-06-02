package crypto.casestudies

import com.espertech.esper.client.Configuration
import com.espertech.esper.client.EPAdministrator
import com.espertech.esper.client.EPRuntime
import com.espertech.esper.client.EPServiceProvider
import com.espertech.esper.client.EPServiceProviderManager
import com.espertech.esper.client.EPStatement
import com.espertech.esper.client.EventBean
import com.espertech.esper.client.UpdateListener
import com.espertech.esper.client.time.CurrentTimeEvent

import scala.beans.BeanProperty
import scala.util._

import crypto._
import crypto.cipher._
import crypto.dsl.Implicits._
import crypto.dsl._

case class CryptoEvent(
  @BeanProperty name: String,
  @BeanProperty encValue: Enc,
  @BeanProperty plainValue: Int
)

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

  epService.getEPAdministrator.createEPL(evenNumbers) += (es =>
    println(s"even: ${es.head.get("plainValue")}"))

  epService.getEPAdministrator.createEPL(smaller100) += (es =>
    println(s"<100: ${es.head.get("plainValue")}"))

  val rand = new Random
  (1 to 100) foreach { n =>
    val randomInt = rand.nextInt(500)
    epService.getEPRuntime.sendEvent(
      CryptoEvent("foo", TheInterpreter.encrypt(randomInt), randomInt))
    Thread.sleep(100)
  }
}

object TheInterpreter {
  val keyRing = KeyRing.create
  val interp = LocalInterpreter(keyRing)

  def encrypt(i: BigInt) = Common.encryptPub(Additive, keyRing)(i).valueOr(sys.error)

  def isEven(e: Enc): Boolean = interp(dsl.isEven(e))

  val onehundred = Common.encrypt(Comparable, keyRing)(100)
  def smaller100(e: Enc): Boolean =interp(e < onehundred)
}
