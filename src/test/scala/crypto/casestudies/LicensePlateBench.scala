package crypto.dsl

import org.scalameter.api._

import crypto._
import crypto.casestudies._
import argonaut._
import Argonaut._

object LicensePlateBench extends CustomPerformanceTest {

  val ns = List(10,50,100,200,500,1000)
  val sizes = Gen.enumeration("events")(ns: _*)

  ns.foreach { n =>
    LicensePlates.main(Array("sim=false",s"num=${n}","gen=true"))
  }

  performance of "license-Plates-Case-Study" in {
    measure method "plain" in {
      using(sizes) in { size =>
        LicensePlates.main(Array("sim=true",s"num=${size}","gen=false"))
      }
    }
  }
}

object LicensePlateEncBench extends CustomPerformanceTest {

  val ns = List(1)
  val sizes = Gen.enumeration("events")(ns: _*)
  val keyRing = Parse.decodeOption[KeyRing](
    io.Source.fromFile(LPConstants.KEYRING_FILE.toString).mkString).getOrElse(
    sys.error("Could not find key file"))

  ns.foreach { n =>
    LicensePlatesEnc.withKeyRing(keyRing)(Array("sim=false",s"num=${n}","gen=true"))
  }

  performance of s"license-Plates-Case-Study" in {
    measure method "encrypted" in {
      using(sizes) in { size =>
        LicensePlatesEnc.withKeyRing(keyRing)(
          Array("sim=true",s"num=${size}","gen=false"))
      }
    }
  }
}
