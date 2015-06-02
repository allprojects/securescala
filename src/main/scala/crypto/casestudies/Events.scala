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
import com.espertech.esper.client.time.TimerControlEvent
import scala.beans.BeanProperty

import crypto._

case class CryptoEvent(
  @BeanProperty name: String,
  @BeanProperty encValue: Enc,
  @BeanProperty plainValue: Int
)

object Esper extends App {
  implicit def λtoListener(f: (Seq[EventBean],Seq[EventBean]) => Unit): UpdateListener =
    new UpdateListener {
      def update(newEvents: Array[EventBean], oldEvents: Array[EventBean]): Unit = {
        f(newEvents,oldEvents)
      }
    }

  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider();
  val expression: String =
    "select avg(plainValue) from crypto.casestudies.CryptoEvent.win:time(30 sec)";
  val statement: EPStatement = epService.getEPAdministrator().createEPL(expression);
}
