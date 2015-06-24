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
// import org.scalacheck._
import scala.beans.BeanProperty
// import scala.io._
import scala.util._
import argonaut._
import Argonaut._
import crypto._

sealed trait LicensePlateEventEnc {
  @BeanProperty def car: EncString
  @BeanProperty def time: Long
  @BeanProperty def speed: EncInt
}

object LicensePlateEventEnc {
  implicit val encoder: EncodeJson[LicensePlateEventEnc] = {
    def generic(e: LicensePlateEventEnc) =
      ("car" := e.car) ->: ("time" := e.time) ->: ("speed" := e.speed) ->: jEmptyObject

    EncodeJson {
      (e: LicensePlateEventEnc) => e match {
        case CarStartEventEnc(_,_,_) => ("type" := "start") ->: generic(e)
        case CheckPointEventEnc(_,_,_,n) => ("type" := "cp"+n) ->: generic(e)
        case CarGoalEventEnc(_,_,_) => ("type" := "goal") ->: generic(e)
      }
    }
  }

  def decoder(key: PubKeys): DecodeJson[LicensePlateEventEnc] = {
    implicit val D = EncInt.decode(key)

    DecodeJson {
      c => for {
        car <- (c --\ "car").as[EncString]
        time <- (c --\ "time").as[Long]
        speed <- (c --\ "speed").as[EncInt]
        typ <- (c --\ "type").as[String]
      } yield typ match {
        case "start" => CarStartEventEnc(car,time,speed)
        case "cp1" | "cp2" | "cp3" =>
          CheckPointEventEnc(car,time,speed,typ.last.toString.toInt)
        case "goal" => CarGoalEventEnc(car,time,speed)
      }
    }
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

object LicensePlatesEnc extends App with EsperImplicits {
  val config: Configuration = new Configuration
  config.addImport("crypto.casestudies.*")

  config.addEventType(classOf[CarStartEventEnc])
  config.addEventType(classOf[CheckPointEventEnc])
  config.addEventType(classOf[CarGoalEventEnc])

  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider(config)
  val rt = epService.getEPRuntime

  rt.sendEvent(new TimerControlEvent(TimerControlEvent.ClockType.CLOCK_EXTERNAL))

  val rng = new Random
  val admin = epService.getEPAdministrator

  val speeders = admin.createEPL("""
INSERT INTO Speeders
SELECT car AS license, number, speed
FROM CheckPointEventEnc
WHERE speed > 133""")

  speeders += { es =>
    println(f"*FLASH* ${es.head.get("license")}%-9s " +
      s"(${es.head.get("speed")}km/h) " +
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

  // LicensePlateData.readEventsDef.foreach(sendEvent)
}
