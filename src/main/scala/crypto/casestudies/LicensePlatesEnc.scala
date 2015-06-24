// The three checkpoint events correspond to checkpoints on a
// street from A to B
// Cars with a license plate move along the street and are captured
// by cameras, scanning the license plate and triggering an event
//
// Visually:
//  S             1         2         3 G
//  T   ----------|---------|---------| O
//  A ->  -   -   -   -   -   -   -   | A
//  R   ----------|---------|---------| L
//  T
package crypto.casestudies

import com.espertech.esper.client._
import com.espertech.esper.client.time._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import scala.beans.BeanProperty
import crypto._
import crypto.cipher._
import crypto.dsl._
import crypto.dsl.Implicits._

sealed trait LicensePlateEventEnc {
  @BeanProperty def car: EncString
  @BeanProperty def time: Long
  @BeanProperty def speed: EncInt
}

final case class CarStartEventEnc(
  @BeanProperty car: EncString,
  @BeanProperty time: Long,
  @BeanProperty speed: EncInt
) extends LicensePlateEventEnc

final case class CheckPointEventEnc(
  @BeanProperty car: EncString,
  @BeanProperty time: Long,
  @BeanProperty speed: EncInt,
  @BeanProperty number: Int
) extends LicensePlateEventEnc

final case class CarGoalEventEnc(
  @BeanProperty car: EncString,
  @BeanProperty time: Long,
  @BeanProperty speed: EncInt
) extends LicensePlateEventEnc

object LicensePlatesEnc extends App with EsperImplicits {
  val config: Configuration = new Configuration
  config.addImport("crypto.casestudies.*")

  config.addEventType(classOf[CarStartEventEnc])
  config.addEventType(classOf[CheckPointEventEnc])
  config.addEventType(classOf[CarGoalEventEnc])

  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider(config)
  val rt = epService.getEPRuntime

  rt.sendEvent(new TimerControlEvent(TimerControlEvent.ClockType.CLOCK_EXTERNAL))

  val admin = epService.getEPAdministrator

  val speeders = admin.createEPL("""
INSERT INTO Speeders
SELECT car AS license, number, speed
FROM CheckPointEventEnc
WHERE Interp.isTooFast(speed)""")

  speeders += { es =>
    println(
      f"${Interp.decryptStr(es.head.get("license").asInstanceOf[EncString])}%-9s " +
      s"(${Interp.decrypt(es.head.get("speed").asInstanceOf[EncInt])}km/h) " +
      s"at checkpoint ${es.head.get("number")}")
  }

  val completions = admin.createEPL("""
INSERT INTO CompleteCarRun
SELECT s.time as startTime,
       g.time as goalTime,
       s.car as car,
       g.time as goalTime,
       s.time as startTime,
       g.speed as maxSpeed
FROM PATTERN [ every s=CarStartEventEnc
               -> c1=CheckPointEventEnc(car=s.car,number=1)
               -> c2=CheckPointEventEnc(car=c1.car,number=2)
               -> c3=CheckPointEventEnc(car=c2.car,number=3)
               -> g=CarGoalEventEnc(car=c3.car)
             ]
""")

  def sendEvent(e: LicensePlateEventEnc): Unit = {
    if (rt.getCurrentTime != e.time) { // avoid duplicates
      rt.sendEvent(new CurrentTimeEvent(e.time))
    }
    rt.sendEvent(e)
  }

  val N = 1000
  val k = Interp.keyRing
  println(s"Generating events for ${N} different cars...")
  val evts = LicensePlateDataEnc.genEventsEnc(k)(N)
  println("done!")
  println(System.currentTimeMillis)
  evts.foreach(sendEvent)
  println(System.currentTimeMillis)
}

object Interp {
  val keyRing = KeyRing.create
  val interpret = LocalInterpreter(keyRing)

  val speedLimit = Common.encrypt(Comparable, keyRing)(133)
  def isTooFast(s: EncInt): Boolean = interpret(s > speedLimit)
  val decryptStr = Common.decryptStr(keyRing)
  val decrypt = Common.decrypt(keyRing)
}

object LicensePlateDataEnc {
  def encryptEvent(k: KeyRing)(e: LicensePlateEvent): LicensePlateEventEnc = {
    val encCar: EncString = Common.encryptStrOpe(k)(e.car)
    val encSpeed: EncInt = Common.depEncrypt(Comparable,k)(e.speed)

    e match {
      case CarStartEvent(_,time,_) => CarStartEventEnc(encCar,time,encSpeed)
      case CheckPointEvent(_,time,_,number) =>
        CheckPointEventEnc(encCar,time,encSpeed,number)
      case CarGoalEvent(_,time,_) => CarGoalEventEnc(encCar,time,encSpeed)
    }
  }

  def genEventsEnc(k: KeyRing)(n: Int) = {
    LicensePlateData.genEvents(n).par.map(encryptEvent(k))
  }
}
