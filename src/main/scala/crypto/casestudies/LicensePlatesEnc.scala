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

import argonaut._
import Argonaut._

import com.espertech.esper.client._
import com.espertech.esper.client.time._
import crypto._
import crypto.cipher._
import crypto.dsl.Implicits._
import crypto.dsl._
import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import scala.beans.BeanProperty
import scala.io._
import scalaz.Ordering._
import scalaz.std.list._
import scalaz.syntax.traverse._

sealed trait LicensePlateEventEnc {
  @BeanProperty def car: EncString
  @BeanProperty def time: Long
  @BeanProperty def speed: EncInt
}

object LicensePlateEventEnc_GenerateKey extends App {
  import Constants._
  Files.deleteIfExists((new File(EVENT_FILE)).toPath)
  val keyRing = KeyRing.create
  Files.write(Paths.get(KEYRING_FILE),
    keyRing.asJson.spaces2.getBytes(StandardCharsets.UTF_8))
}

object LicensePlateEventEnc {
  implicit def codec(
    implicit encInt: DecodeJson[EncInt]): CodecJson[LicensePlateEventEnc] = {

    def generic(e: LicensePlateEventEnc) =
      ("car_enc" := e.car) ->: ("time" := e.time) ->: ("speed_enc" := e.speed) ->: jEmptyObject

    CodecJson(
      (e: LicensePlateEventEnc) => e match {
        case CarStartEventEnc(_,_,_) => ("type" := "start") ->: generic(e)
        case CheckPointEventEnc(_,_,_,n) => ("type" := "cp"+n) ->: generic(e)
        case CarGoalEventEnc(_,_,_) => ("type" := "goal") ->: generic(e)
      } ,
      c => for {
        car <- (c --\ "car_enc").as[EncString]
        time <- (c --\ "time").as[Long]
        speed <- (c --\ "speed_enc").as[EncInt]
        typ <- (c --\ "type").as[String]
      } yield typ match {
        case "start" => CarStartEventEnc(car,time,speed)
        case "cp1" | "cp2" | "cp3" =>
          CheckPointEventEnc(car,time,speed,typ.last.toString.toInt)
        case "goal" => CarGoalEventEnc(car,time,speed)
      }
    )
  }
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

private object Constants {
  val NUM_EVENTS = 1000
  val EVENT_FILE = "license-plate-events-enc.json"
  val KEYRING_FILE = "license-plate-events-enc.keys"
}

object LicensePlatesEnc extends App with EsperImplicits {
  import Constants._

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
SELECT car AS license, number, speed
FROM CheckPointEventEnc
WHERE Interp.isTooFast(speed)""")

  speeders += { es =>
    println("*FLASH*: " +
      f"${Interp.decryptStr(es.head.get("license").asInstanceOf[EncString])}%-9s " +
      s"(${Interp.decrypt(es.head.get("speed").asInstanceOf[EncInt])}km/h) " +
      s"at checkpoint ${es.head.get("number")}")
  }

  val completions = admin.createEPL("""
SELECT s.time as startTime,
       g.time as goalTime,
       s.car as car,
       g.time - s.time as duration,
       Interp.max(s.speed,c1.speed,c2.speed,c3.speed,g.speed) as maxSpeed
FROM PATTERN [ every s=CarStartEventEnc
               -> c1=CheckPointEventEnc(Interp.strEq(car,s.car),number=1)
               -> c2=CheckPointEventEnc(Interp.strEq(car,c1.car),number=2)
               -> c3=CheckPointEventEnc(Interp.strEq(car,c2.car),number=3)
               -> g=CarGoalEventEnc(Interp.strEq(car,c3.car))
             ]
""")

  completions += { (es: Seq[EventBean]) =>
    println(
      f"${Interp.decryptStr(es.head.get("car").asInstanceOf[EncString])}%-9s " +
      f"completed in ${es.head.get("duration").asInstanceOf[Long] / 1000}%ss " +
      f"with speed ${Interp.decrypt(es.head.get("maxSpeed").asInstanceOf[EncInt])}%3s")
  }

  generateEventsIfRequired()
  val keyRing = Parse.decodeOption[KeyRing](
    Source.fromFile(KEYRING_FILE).mkString).get

  implicit val decodeInt = EncInt.decode(keyRing)
  val evts = Parse.decodeOption[List[LicensePlateEventEnc]](
    Source.fromFile(EVENT_FILE).mkString).get

  val start = System.currentTimeMillis
  evts.foreach(sendEvent)
  val end = System.currentTimeMillis
  println(s"Time for event processing: ${(end - start) / 1000.0}s")

  def sendEvent(e: LicensePlateEventEnc): Unit = {
    if (rt.getCurrentTime != e.time) { // avoid duplicates
      rt.sendEvent(new CurrentTimeEvent(e.time))
    }
    rt.sendEvent(e)
  }

  def generateEventsIfRequired(): Unit = {
    if (args.isEmpty && new File(EVENT_FILE).exists) {
      println("Found event file.")
    } else {
      if (! (new File(KEYRING_FILE).exists)) {
        sys.error("Could not find keyfile, did you generate it in advance?")
      }
      val keyRing = Parse.decodeOption[KeyRing](
        Source.fromFile(KEYRING_FILE).mkString).getOrElse(
        sys.error(s"Could not parse ${KEYRING_FILE}"))

      println(s"Generating events for ${NUM_EVENTS} different cars...")
      val evts = LicensePlateDataEnc.genEventsEnc(keyRing)(NUM_EVENTS)

      Files.write(Paths.get(EVENT_FILE),
        evts.asJson.spaces2.getBytes(StandardCharsets.UTF_8))

      println(s"done! Generated ${evts.size} events")
    }
  }
}

object Interp {
  import Constants._
  val keyRing = {
    if (! (new File(KEYRING_FILE).exists)) {
      sys.error("Could not find keyfile, did you generate it in advance?")
    }

    Parse.decodeOption[KeyRing](
      Source.fromFile(KEYRING_FILE).mkString).getOrElse(
      sys.error(s"Could not parse ${KEYRING_FILE}"))
  }

  val interpret = LocalInterpreter(keyRing)

  val speedLimit = Common.encrypt(Comparable, keyRing)(133)
  def isTooFast(s: EncInt): Boolean = interpret(s > speedLimit)

  def strEq(s1: EncString, s2: EncString) = interpret(s1 ?|? s2) == EQ
  def max(i1: EncInt, i2: EncInt, i3: EncInt, i4: EncInt, i5: EncInt) =
    interpret(List(i1,i2,i3,i4,i5).traverse(toOpe).map(_.max))

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
    LicensePlateData.genEvents(n).map(encryptEvent(k))
  }
}
