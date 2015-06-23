package crypto.casestudies

import com.espertech.esper.client._
import com.espertech.esper.client.time._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import org.scalacheck._
import scala.beans.BeanProperty
import scala.io._
import scala.util._

final case class Car(@BeanProperty license: String)
final case class TimeStamp(@BeanProperty unwrap: Long) {
  def advance(d: Long): TimeStamp = TimeStamp(unwrap + d)
}

object TimeStamp {
  implicit val instance: Ordering[TimeStamp] = new Ordering[TimeStamp] {
    override def compare(x: TimeStamp, y: TimeStamp): Int =
      implicitly[Ordering[Long]].compare(x.unwrap,y.unwrap)
  }
}

trait Clock { def now: TimeStamp }
case object DefaultClock extends Clock {
  def now = TimeStamp(System.currentTimeMillis)
}

// The three checkpoint events correspond to checkpoints on a
// street from A to B
// Cars with a license plate move along the street and are captured
// by cameras, scanning the license plate and triggering an event of the license
// plate together with the timestamp
//
// Visually:
//             1         2         3
//   ----------|---------|---------|-
//  A  -   -   -   -   - -   -   -   B
//   ----------|---------|---------|-
//

sealed trait LicensePlateEvent {
  @BeanProperty def car: Car
  @BeanProperty def time: TimeStamp
}

final case class CarStartEvent(
  @BeanProperty car: Car,
  @BeanProperty time: TimeStamp
) extends LicensePlateEvent

final case class C1Event(
  @BeanProperty car: Car,
  @BeanProperty time: TimeStamp
) extends LicensePlateEvent

final case class C2Event(
  @BeanProperty car: Car,
  @BeanProperty time: TimeStamp
) extends LicensePlateEvent

final case class C3Event(
  @BeanProperty car: Car,
  @BeanProperty time: TimeStamp
) extends LicensePlateEvent

final case class CarGoalEvent(
  @BeanProperty car: Car,
  @BeanProperty time: TimeStamp
) extends LicensePlateEvent

object LP {
  def difference(tx: TimeStamp, ty: TimeStamp) =
    TimeStamp(tx.unwrap.max(ty.unwrap) - tx.unwrap.min(ty.unwrap))
}

object LicensePlates extends App with EsperImplicits {
  val config: Configuration = new Configuration
  config.addImport("crypto.casestudies.*")

  config.addEventType(classOf[CarStartEvent])
  config.addEventType(classOf[C1Event])
  config.addEventType(classOf[C2Event])
  config.addEventType(classOf[C3Event])
  config.addEventType(classOf[CarGoalEvent])

  val epService: EPServiceProvider = EPServiceProviderManager.getDefaultProvider(config)
  val rt = epService.getEPRuntime

  rt.sendEvent(new TimerControlEvent(TimerControlEvent.ClockType.CLOCK_EXTERNAL))

  val rng = new Random
  val admin = epService.getEPAdministrator

  admin.createEPL("""
INSERT INTO CompleteCarRun
SELECT s.time as startTime,
       g.time as goalTime,
       s.car as car,
       LP.difference(g.time,s.time) as duration
FROM PATTERN [ every s=CarStartEvent
               -> c1=C1Event(car=s.car)
               -> c2=C2Event(car=c1.car)
               -> c3=C3Event(car=c2.car)
               -> g=CarGoalEvent(car=c3.car)
             ]
""") += { (es: Seq[EventBean]) =>
    println(f"${es.head.get("car").asInstanceOf[Car].license}%-9s completed in ${es.head.get("duration")}%s")
  }

  def sendEvent(e: LicensePlateEvent): Unit = {
    if (rt.getCurrentTime != e.time.unwrap) { // avoid duplicates
      rt.sendEvent(new CurrentTimeEvent(e.time.unwrap))
    }
    rt.sendEvent(e)
  }

  LicensePlateData.readEventsDef.foreach(sendEvent)
}

object LicensePlateData {
  val FILE_NAME = "license-plates.csv"

  def main(args: Array[String]) = {
    val N = 1000
    println(s"Generating events for ${N} different cars...")
    val rng = new Random

    val plates = Gen.listOfN(N, Arbitrary.arbitrary[Car]).sample.get
    val evts = plates.flatMap(genEvtsFor(rng))
    writeEvents(FILE_NAME)(evts)

    println("done!")
  }

  val SEPARATOR = ","
  private def genericEventToLine(e: LicensePlateEvent): String =
    e.time.unwrap + SEPARATOR + e.car.license

  private def genEvtsFor(rng:Random)(plate:Car): Seq[LicensePlateEvent] = {
    def now = TimeStamp(0)
    def rndDelay = rng.nextInt(6000*1000).toLong + (3000*1000)

    val ts: Seq[TimeStamp] =
      Stream.iterate(now.advance(rng.nextInt(100000*1000).toLong),5)(_.advance(rndDelay))

    Seq(
      CarStartEvent(plate,ts(0)),
      C1Event(plate,ts(1)),
      C2Event(plate,ts(2)),
      C3Event(plate,ts(3)),
      CarGoalEvent(plate,ts(4))
    )
  }

  private def eventToLine(e: LicensePlateEvent): String = e match {
    case CarStartEvent(p,t) => genericEventToLine(e) + SEPARATOR + "start"
    case C1Event(p,t) => genericEventToLine(e) + SEPARATOR + "c1"
    case C2Event(p,t) => genericEventToLine(e) + SEPARATOR + "c2"
    case C3Event(p,t) => genericEventToLine(e) + SEPARATOR + "c3"
    case CarGoalEvent(p,t) => genericEventToLine(e) + SEPARATOR + "goal"
  }

  def writeEvents(fileName: String)(e: Seq[LicensePlateEvent]): Unit = {
    val fileContent = e.sortBy(_.time).map(eventToLine).mkString("\n")
    Files.write(
      Paths.get(fileName),
      fileContent.getBytes(StandardCharsets.UTF_8))

    ()
  }

  private def parseEvent(s: String): LicensePlateEvent = {
    val Array(t,p,n) = s.split(",")
    n match {
      case "start" => CarStartEvent(Car(p),TimeStamp(t.toLong))
      case "c1" => C1Event(Car(p),TimeStamp(t.toLong))
      case "c2" => C2Event(Car(p),TimeStamp(t.toLong))
      case "c3" => C3Event(Car(p),TimeStamp(t.toLong))
      case "goal" => CarGoalEvent(Car(p),TimeStamp(t.toLong))
    }
  }

  def readEvents(fileName: String): Seq[LicensePlateEvent] = {
    Source.fromFile(fileName).getLines.map(parseEvent).toSeq
  }

  def readEventsDef: Seq[LicensePlateEvent] = readEvents(FILE_NAME)
}

object Car {
  private val MAX_LICENSE_PLATE_LENGTH = 8

  implicit val arbitraryLicensePlate: Arbitrary[Car] = Arbitrary[Car] {
    for {
      prefix <- Gen.oneOf(prefixes)
      numLetters <- Gen.oneOf(1,2)
      letters <- Gen.listOfN(numLetters, Gen.oneOf('A' to 'Z'))
      remainingSlots = MAX_LICENSE_PLATE_LENGTH - prefix.length - numLetters
      digits <- Gen.listOfN(remainingSlots.min(4), Gen.oneOf(1 to 9))
    } yield Car(prefix + "-" + letters.mkString + digits.mkString)
  }

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
  )
}
