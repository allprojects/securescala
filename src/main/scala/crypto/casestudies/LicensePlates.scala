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
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import org.scalacheck._
import scala.beans.BeanProperty
import scala.io.Source
import scala.util._

sealed trait LicensePlateEvent {
  @BeanProperty def car: String
  @BeanProperty def time: Long
  @BeanProperty def speed: Int
}

object LicensePlateEvent {
  implicit val codec: CodecJson[LicensePlateEvent] = {

    def generic(e: LicensePlateEvent) =
      ("car" := e.car) ->: ("time" := e.time) ->: ("speed" := e.speed) ->: jEmptyObject

    CodecJson(
      (e: LicensePlateEvent) => e match {
        case CarStartEvent(_,_,_) => ("type" := "start") ->: generic(e)
        case CheckPointEvent(_,_,_,n) => ("type" := "cp"+n) ->: generic(e)
        case CarGoalEvent(_,_,_) => ("type" := "goal") ->: generic(e)
      } ,
      c => for {
        car <- (c --\ "car").as[String]
        time <- (c --\ "time").as[Long]
        speed <- (c --\ "speed").as[Int]
        typ <- (c --\ "type").as[String]
      } yield typ match {
        case "start" => CarStartEvent(car,time,speed)
        case "cp1" | "cp2" | "cp3" =>
          CheckPointEvent(car,time,speed,typ.last.toString.toInt)
        case "goal" => CarGoalEvent(car,time,speed)
      }
    )
  }
}

final case class CarStartEvent(
  @BeanProperty car: String,
  @BeanProperty time: Long,
  @BeanProperty speed: Int
) extends LicensePlateEvent

final case class CheckPointEvent(
  @BeanProperty car: String,
  @BeanProperty time: Long,
  @BeanProperty speed: Int,
  @BeanProperty number: Int
) extends LicensePlateEvent

final case class CarGoalEvent(
  @BeanProperty car: String,
  @BeanProperty time: Long,
  @BeanProperty speed: Int
) extends LicensePlateEvent

object LicensePlates extends App with EsperImplicits {
  val args_ = args.map(_.split("""=""") match {
    case Array(x,y) => List((x,y))
    case _ => List()
  }).flatten.toMap
  val NUM_EVENTS = args_.get("num").map(_.toInt).getOrElse(1000)
  val EVENT_FILE = args_.get("file").getOrElse(s"license-plate-events-${NUM_EVENTS}.json")
  val GENERATE_EVTS = args_.get("gen").map(_.toBoolean).getOrElse(false)
  val RUN_SIMULATION = args_.get("sim").map(_.toBoolean).getOrElse(true)

  val config: Configuration = new Configuration
  config.addImport("crypto.casestudies.*")

  config.addEventType(classOf[CarStartEvent])
  config.addEventType(classOf[CheckPointEvent])
  config.addEventType(classOf[CarGoalEvent])

  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider(config)
  val rt = epService.getEPRuntime

  rt.sendEvent(new TimerControlEvent(TimerControlEvent.ClockType.CLOCK_EXTERNAL))

  val admin = epService.getEPAdministrator

  val speeders = admin.createEPL("""
SELECT car AS license, number, speed
FROM CheckPointEvent
WHERE speed > 133""")

  speeders += { es =>
    println(f"*FLASH* ${es.head.get("license")}%-9s " +
      s"(${es.head.get("speed")}km/h) " +
      s"at checkpoint ${es.head.get("number")}")
  }

  val completions = admin.createEPL("""
SELECT s.time as startTime,
       g.time as goalTime,
       s.car as car,
       g.time - s.time as duration,
       max(s.speed,c1.speed,c2.speed,c3.speed,g.speed) as maxSpeed
FROM PATTERN [ every s=CarStartEvent
               -> c1=CheckPointEvent(car=s.car,number=1)
               -> c2=CheckPointEvent(car=c1.car,number=2)
               -> c3=CheckPointEvent(car=c2.car,number=3)
               -> g=CarGoalEvent(car=c3.car)
             ]
""")

  completions += { (es: Seq[EventBean]) =>
    println(
      f"${es.head.get("car")}%-9s " +
      f"completed in ${es.head.get("duration").asInstanceOf[Long] / 1000}%ss " +
      f"with speed ${(es.head.get("maxSpeed"))}%3s")
  }

  generateEventsIfRequired()

  if (!RUN_SIMULATION) {
    println("Requested to not run simulation, quitting.")
    System.exit(0)
  }
  val evts = Parse.decodeOption[List[LicensePlateEvent]](
    Source.fromFile(EVENT_FILE).mkString).get

  val start = System.currentTimeMillis
  evts.foreach(sendEvent)
  val end = System.currentTimeMillis
  println(s"Time for event processing: ${(end - start) / 1000.0}s")

  def sendEvent(e: LicensePlateEvent): Unit = {
    if (rt.getCurrentTime != e.time) { // avoid duplicates
      rt.sendEvent(new CurrentTimeEvent(e.time))
    }
    rt.sendEvent(e)
  }

  def generateEventsIfRequired(): Unit = {
    if (!GENERATE_EVTS && new File(EVENT_FILE).exists) {
      println("Found event file.")
    } else {
      print(s"Generating events for ${NUM_EVENTS} different cars...")

      val evts = LicensePlateData.genEvents(NUM_EVENTS).sortBy(_.time)

      Files.write(Paths.get(EVENT_FILE),
        evts.asJson.spaces2.getBytes(StandardCharsets.UTF_8))


      println(s"done! Generated ${evts.size} events")
    }
  }
}

object LicensePlateData {

  def genEvents(n: Int): List[LicensePlateEvent] = {
    val rng = new Random
    val plates = Gen.listOfN(n, LicensePlate.plateGen).sample.get
    plates.flatMap(LicensePlateData.genEvtsFor(rng))
  }

  def genEvtsFor(rng:Random)(plate:String): Seq[LicensePlateEvent] = {
    def now = 0
    def rndDelay = rng.nextInt(6*1000).toLong + rng.nextInt(12*1000).toLong

    val ts: Seq[Long] =
      Stream.iterate(now + (rng.nextInt(10*1000).toLong),5)(_ + rndDelay)

    val speeds: Seq[Int] = List.fill(5)(
      100 +
        rng.nextInt( 5) +
        rng.nextInt(10) +
        rng.nextInt(10) +
        rng.nextInt(10) +
        rng.nextInt(10))

    Seq(
      CarStartEvent(plate,ts(0),speeds(0)),
      CheckPointEvent(plate,ts(1),speeds(1),1),
      CheckPointEvent(plate,ts(2),speeds(2),2),
      CheckPointEvent(plate,ts(3),speeds(3),3),
      CarGoalEvent(plate,ts(4),speeds(4))
    )
  }
}

object LicensePlate {
  private val MAX_LICENSE_PLATE_LENGTH = 8

  implicit def plateGen: Gen[String] =
    for {
      prefix <- Gen.oneOf(prefixes)
      numLetters <- Gen.oneOf(1,2)
      letters <- Gen.listOfN(numLetters, Gen.oneOf('A' to 'Z'))
      remainingSlots = MAX_LICENSE_PLATE_LENGTH - prefix.length - numLetters
      digits <- Gen.listOfN(remainingSlots.min(4), Gen.oneOf(1 to 9))
    } yield (prefix + "-" + letters.mkString + digits.mkString)

  private val prefixes = Seq(
    "A","AA","AB","ABG","ABI","AC","AE","AH","AIB","AIC",
    "AK","ALF","ALZ","AM","AN","ANA","ANG","ANK","AÖ","AP",
    "APD","ARN","ART","AS","ASL","ASZ","AT","AU","AUR","AW",
    "AZ","AZE","B","BA","BAD","BAR","BB","BBG","BBL","BC",
    "BCH","BD","BE","BED","BER","BF","BG","BGL","BH","BI",
    "BID","BIN","BIR","BIT","BIW","BK","BKS","BL","BLB","BLK",
    "BM","BN","BNA","BO","BÖ","BOH","BOR","BOT","BP","BRA",
    "BRB","BRG","BRK","BRL","BRV","BS","BT","BTF","BÜD","BÜS",
    "BÜZ","BW","BWL","BYL","BZ","C","CA","CAS","CB","CE",
    "CHA","CLP","CLZ","CO","COC","COE","CUX","CW","D","DA",
    "DAH","DAN","DAU","DB","DBR","DD","DE","DEG","DEL","DGF",
    "DH","DI","DIL","DIN","DIZ","DKB","DL","DLG","DM","DN",
    "DO","DON","DU","DÜW","DW","DZ","E","EA","EB","EBE",
    "EBN","EBS","ECK","ED","EE","EF","EG","EI","EIC","EIL",
    "EIN","EIS","EL","EM","EMD","EMS","EN","ER","ERB","ERH",
    "ERK","ERZ","ES","ESB","ESW","EU","EW","F","FB","FD",
    "FDB","FDS","FEU","FF","FFB","FG","FI","FKB","FL","FLÖ",
    "FN","FO","FOR","FR","FRG","FRI","FRW","FS","FT","FTL",
    "FÜ","FÜS","G","GA","GAN","GAP","GC","GD","GDB","GE",
    "GEL","GEO","GER","GF","GG","GHA","GHC","GI","GK","GL",
    "GM","GMN","GNT","GÖ","GOA","GOH","GP","GR","GRA","GRH",
    "GRI","GRM","GRZ","GS","GT","GTH","GÜ","GUB","GUN","GVM",
    "GW","GZ","H","HA","HAB","HAL","HAM","HAS","HB","HBN",
    "HBS","HC","HCH","HD","HDH","HDL","HE","HEB","HEF","HEI",
    "HEL","HER","HET","HF","HG","HGN","HGW","HH","HHM","HI",
    "HIG","HIP","HK","HL","HM","HMÜ","HN","HO","HOG","HOH",
    "HOL","HOM","HOR","HOT","HP","HR","HRO","HS","HSK","HST",
    "HU","HV","HVL","HWI","HX","HY","HZ","IGB","IK","IL",
    "ILL","IN","IZ","J","JE","JL","JÜL","K","KA","KB",
    "KC","KE","KEH","KEL","KEM","KF","KG","KH","KI","KIB",
    "KK","KL","KLE","KLZ","KM","KN","KO","KÖN","KÖT","KR",
    "KRU","KS","KT","KU","KÜN","KUS","KY","KYF","L","LA",
    "LAU","LB","LBS","LBZ","LD","LDK","LDS","LEO","LER","LEV",
    "LG","LH","LI","LIB","LIF","LIP","LL","LM","LÖ","LÖB",
    "LOS","LP","LR","LRO","LSA","LSN","LSZ","LU","LÜN","LUP",
    "LWL","M","MA","MAB","MAI","MAK","MAL","MB","MC","MD",
    "ME","MEI","MEK","MER","MET","MG","MGH","MGN","MH","MHL",
    "MI","MIL","MK","MKK","ML","MM","MN","MO","MOD","MOL",
    "MON","MOS","MQ","MR","MS","MSE","MSH","MSP","MST","MTL",
    "MTK","MÜ","MÜB","MÜR","MVL","MW","MY","MYK","MZ","MZG",
    "N","NAB","NAI","NB","ND","NDH","NE","NEA","NEB","NEC",
    "NEN","NES","NEW","NF","NH","NI","NK","NL","NM","NMB",
    "NMS","NÖ","NOH","NOL","NOM","NOR","NP","NR","NRW","NU",
    "NVP","NW","NWM","NY","NZ","OA","OAL","OB","OBG","OC",
    "OCH","OD","OE","OF","OG","OH","OHA","OHV","OHZ","OK",
    "OL","OPR","OR","OS","OSL","OVI","OVL","OZ","P","PA",
    "PAF","PAN","PAR","PB","PCH","PE","PEG","PF","PI","PIR",
    "PL","PLÖ","PM","PN","PR","PRÜ","PS","PW","PZ","QFT",
    "QLB","R","RA","RC","RD","RDG","RE","REG","REH","RG",
    "RH","RI","RID","RIE","RL","RM","RO","ROD","ROF","ROK",
    "ROL","ROS","ROT","ROW","RP","RPL","RS","RSL","RT","RU",
    "RÜD","RÜG","RV","RW","RZ","S","SAB","SAD","SAL","SAN",
    "SAW","SB","SBG","SBK","SC","SCZ","SDH","SDL","SDT","SE",
    "SEB","SEE","SEF","SEL","SFB","SFT","SG","SGH","SH","SHA",
    "SHG","SHK","SHL","SI","SIG","SIM","SK","SL","SLE","SLF",
    "SLK","SLN","SLS","SLÜ","SLZ","SM","SN","SO","SOB","SOG",
    "SOK","SÖM","SON","SP","SPB","SPN","SR","SRB","SRO","ST",
    "STA","STB","STD","STE","STL","SU","SUL","SÜW","SW","SWA",
    "SZ","SZB","TBB","TDO","TE","TET","TF","TG","THL","THW",
    "TIR","TO","TÖL","TP","TR","TS","TÜ","TUT","UE","UEM",
    "UFF","UH","UL","UM","UN","USI","V","VAI","VB","VEC",
    "VER","VG","VIB","VIE","VK","VOH","VR","VS","W","WA",
    "WAF","WAK","WAN","WAT","WB","WBS","WDA","WE","WEL","WEN",
    "WER","WES","WF","WHV","WI","WIL","WIS","WIT","WK","WL",
    "WLG","WM","WMS","WN","WND","WO","WOB","WOH","WOL","WOR",
    "WOS","WR","WRN","WS","WSF","WST","WSW","WT","WTM","WÜ",
    "WUG","WÜM","WUN","WUR","WW","WZ","WIZ","WZL","X","Y",
    "Z","ZE","ZEL","ZI","ZP","ZR","ZW","ZZ"
  ).filterNot(s => "ÄäÖöÜü".toList.exists(s.contains(_)))
}
