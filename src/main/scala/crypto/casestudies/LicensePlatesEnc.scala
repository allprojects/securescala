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
import org.scalacheck._
import scala.beans.BeanProperty
import scala.io._
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

  LicensePlateData.readEventsDef.foreach(sendEvent)
}

object LicensePlateData {
  val FILE_NAME = "license-plates.json"

  def main(args: Array[String]) = {
    val N = 1000
    println(s"Generating events for ${N} different cars...")
    val rng = new Random

    val plates = Gen.listOfN(N, Car.plateGen).sample.get
    val evts = plates.flatMap(genEvtsFor(rng))
    writeEvents(FILE_NAME)(evts)

    println("done!")
  }

  val SEPARATOR = ","

  private def genEvtsFor(rng:Random)(plate:EncString): Seq[LicensePlateEventEnc] = {
    def now = 0
    def rndDelay = rng.nextInt(600*1000).toLong + rng.nextInt(120*1000).toLong

    val ts: Seq[Long] =
      Stream.iterate(now + (rng.nextInt(10000*1000).toLong),5)(_ + rndDelay)

    val speeds: Seq[EncInt] = List.fill(5)(
      100 +
        rng.nextInt( 5) +
        rng.nextInt(10) +
        rng.nextInt(10) +
        rng.nextInt(10) +
        rng.nextInt(10))

    Seq(
      CarStartEventEnc(plate,ts(0),speeds(0)),
      CheckPointEventEnc(plate,ts(1),speeds(1),1),
      CheckPointEventEnc(plate,ts(2),speeds(2),2),
      CheckPointEventEnc(plate,ts(3),speeds(3),3),
      CarGoalEventEnc(plate,ts(4),speeds(4))
    )
  }

  def writeEvents(fileName: String)(es: List[LicensePlateEventEnc]): Unit = {
    val fileContent: String = es.sortBy(_.time).asJson.nospaces

    Files.write(
      Paths.get(fileName),
      fileContent.getBytes(StandardCharsets.UTF_8))

    ()
  }

  def readEvents(keys: PubKeys)(fileName: String) = {
    implicit val D = LicensePlateEventEnc.decoder(keys)
    Parse.decodeOption[List[LicensePlateEventEnc]](Source.fromFile(fileName).mkString).get
  }

  def readEventsDef: List[LicensePlateEventEnc] = readEvents(FILE_NAME)
}

object Car {
  private val MAX_LICENSE_PLATE_LENGTH = 8

  implicit def plateGen: Gen[EncString] =
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
  )
}
