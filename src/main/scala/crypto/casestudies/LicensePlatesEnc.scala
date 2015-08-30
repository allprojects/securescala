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
import crypto.remote._
import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import scala.beans.BeanProperty
import scala.concurrent._
import scala.concurrent.duration._
import scalaz.Ordering._
import scalaz._
import scalaz.std.list._
import scalaz.syntax.traverse._

sealed trait LicensePlateEventEnc {
  @BeanProperty def car: EncString
  @BeanProperty def time: Long
  @BeanProperty def speed: EncInt
}

object LicensePlateEventEnc_GenerateKey extends App {
  import LPConstants._
  val keyRing = KeyRing.create

  Paths.get(".").
    toAbsolutePath.
    toFile.
    listFiles.
    map(_.toString).
    filter(_ matches """.*events-enc-\d+.*\.json""").
    map(Paths.get(_)).
    foreach(Files.deleteIfExists(_))

  Files.write(KEYRING_FILE,
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

object LPConstants {
  val KEYRING_FILE = Paths.get("license-plate-events-enc.keys").toAbsolutePath
}

object NullOutputStream extends OutputStream {override def write(b: Int) = ()}

object LicensePlatesEnc extends EsperImplicits {
  def withKeyRing(keyRing: KeyRing)(args: Array[String]) = {
    val args_ = args.map(_.split("""=""") match {
      case Array(x,y) => List((x,y))
      case _ => List()
    }).flatten.toMap
    val NUM_EVENTS = args_.get("num").map(_.toInt).getOrElse(1000)
    val EVENT_FILE =
      args_.get("file").getOrElse(s"license-plate-events-enc-${NUM_EVENTS}.json")
    val GENERATE_EVTS = args_.get("gen").map(_.toBoolean).getOrElse(false)
    val RUN_SIMULATION = args_.get("sim").map(_.toBoolean).getOrElse(true)
    val OUTPUT = args_.get("print").map(_.toBoolean).map { doPrint =>
      if (doPrint) {
        System.out
      } else {
        new java.io.PrintStream(NullOutputStream)
      }
    }.getOrElse(new java.io.PrintStream(NullOutputStream))

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
      OUTPUT.println("*FLASH*: " +
        f"${es.head.get("license").asInstanceOf[EncString]}%s " +
        s"(${es.head.get("speed").asInstanceOf[EncInt]}km/h) " +
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
      OUTPUT.println(
        f"${es.head.get("car").asInstanceOf[EncString]}%s " +
          f"completed in ${es.head.get("duration").asInstanceOf[Long] / 1000}%ss " +
          f"with speed ${es.head.get("maxSpeed").asInstanceOf[EncInt]}%s")
    }

    generateEventsIfRequired()
    if (!RUN_SIMULATION) {
      OUTPUT.println("Requested to not run simulation, quitting.")
    } else {

      implicit val decodeInt = EncInt.decode(keyRing)
      val evts = Parse.decodeOption[List[LicensePlateEventEnc]](
        io.Source.fromFile(EVENT_FILE).mkString).get

      val start = System.currentTimeMillis
      evts.foreach(sendEvent)
      val end = System.currentTimeMillis
      OUTPUT.println(s"Time for event processing: ${(end - start) / 1000.0}s")
      Interp.shutdown(())
    }

    def sendEvent(e: LicensePlateEventEnc): Unit = {
      if (rt.getCurrentTime != e.time) { // avoid duplicates
        rt.sendEvent(new CurrentTimeEvent(e.time))
      }
      rt.sendEvent(e)
    }

    def generateEventsIfRequired(): Unit = {
      implicit val decodeInt = EncInt.decode(keyRing)
      OUTPUT.println("In generateEventsIfRequired")
      if (!GENERATE_EVTS && new File(EVENT_FILE).exists) {
        OUTPUT.println("Found event file.")
      } else {
        OUTPUT.println(s"Generating ${NUM_EVENTS} events...")
        val evts = LicensePlateDataEnc.genEventsEnc(keyRing)(NUM_EVENTS)

        Files.write(Paths.get(EVENT_FILE),
          evts.asJson.spaces2.getBytes(StandardCharsets.UTF_8))

        OUTPUT.println(s"done! Generated ${evts.size} events")
      }
    }
  }

  def main(args: Array[String]) = {
    val keyRing = Parse.decodeOption[KeyRing](
      io.Source.fromFile(LPConstants.KEYRING_FILE.toString).mkString).getOrElse(
      sys.error("Could not find key file"))

    withKeyRing(keyRing)(args)
  }
}

object Interp {
  import LPConstants._
  val USE_REMOTE = false

  val keyRing = {
    if (! (KEYRING_FILE.toFile.exists)) {
      sys.error("Could not find keyfile, did you generate it in advance?")
    }

    Parse.decodeOption[KeyRing](
      io.Source.fromFile(KEYRING_FILE.toString).mkString).getOrElse(
      sys.error(s"Could not parse ${KEYRING_FILE}"))
  }

  var shutdown: Unit => Unit = (_: Unit) => ()

  val interpret = if (USE_REMOTE) {
    val (system,futService) =
      CryptoService.connect(scala.concurrent.ExecutionContext.Implicits.global)

    val service = \/.fromTryCatchNonFatal(Await.result(futService, 10.seconds)) match {
      case -\/(err) =>
        println(s"Error connecting to crypto service: ${err}")
        println("Did you start the crypto service before running this?")
        system.shutdown()
        sys.exit(1)
      case \/-(service) =>
        println("Connected to crypto service.")
        service
    }

    val keys: PubKeys = Await.result(service.publicKeys, 10.seconds)

    shutdown = _ => system.shutdown()

    Blocking(30.seconds)(
      new RemoteInterpreter(service, keys)(
        scala.concurrent.ExecutionContext.Implicits.global))
  } else {
    new LocalInterpreter(keyRing)
  }

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
