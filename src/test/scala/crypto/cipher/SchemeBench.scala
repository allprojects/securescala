package crypto.cipher

import scala.util._

import crypto._
import org.scalameter.api._

trait SchemeBench[A] { this: PerformanceTest =>
  val encrypt: BigInt => A
  val decrypt: A => BigInt

  def name: String

  def sizes: Gen[Int]
  def lists = for (size <- sizes) yield List.fill(size)(BigInt(32, new Random))
  def encLists = for (list <- lists) yield list.map(encrypt(_))

  performance of name in {
    measure method "encryption" in {
      using(lists) in { xs => xs.map(encrypt(_)) }
    }

    measure method "decryption" in {
      using(encLists) in { xs => xs.map(decrypt(_))}
    }
  }
}

object AesBench extends CustomPerformanceTest with SchemeBench[Array[Byte]] {
  def name = "AES"
  def sizes = Gen.range("sizes")(5000,20000,5000)

  val (aesEncrypt,aesDecrypt) = Aes.create(Aes.B256)

  val encrypt = (x: BigInt) => aesEncrypt(x)
  val decrypt = (x: Array[Byte]) => BigInt(aesDecrypt(x))
}

object OpeBench extends CustomPerformanceTest with SchemeBench[BigInt] {
  def name = "OPE"
  def sizes = Gen.enumeration("sizes")(1, 5, 10, 15)

  val (opeEnc,opeDec,_) = Ope.createNum(128)

  val encrypt = (x: BigInt) => opeEnc(x).valueOr(sys.error)
  val decrypt = (x: BigInt) => opeDec(x)
}

object ElGamalBench
    extends CustomPerformanceTest
    with SchemeBench[(BigInt,BigInt)] {

  def name = "ElGamal"
  def sizes = Gen.enumeration("sizes")(10,25,50,75)

  val (elGamalEnc,elGamalDec,_) = ElGamal.create(1024)

  val encrypt = (x: BigInt) => elGamalEnc(x).valueOr(sys.error)
  val decrypt = (x: (BigInt,BigInt)) => elGamalDec(x._1,x._2)
}

object PaillierBench extends CustomPerformanceTest with SchemeBench[BigInt] {
  def name = "Paillier"
  def sizes = Gen.enumeration("sizes")(1, 5, 10, 15)

  val (paillierEnc, paillierDec, _) = Paillier.create(1024)

  val encrypt = (x: BigInt) => paillierEnc(x).valueOr(sys.error)
  val decrypt = (x: BigInt) => paillierDec(x)
}
