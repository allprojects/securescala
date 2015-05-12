package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

trait ScalaCheckGen {
  val posInt = arbitrary[BigInt] retryUntil (_.signum == 1)
}

case class EncryptedGens(keys: KeyRing) extends ScalaCheckGen {
  val allowedNumber =
    posInt retryUntil (_ < BigInt(2).pow(keys.priv.opePriv.plainBits-1))

  def encryptedNumber: Gen[Enc] = for {
    scheme <- Gen.oneOf(Additive, Multiplicative, Equality, Comparable)
    i <- allowedNumber
  } yield Common.encrypt(scheme, keys)(i)

  def encryptedList(maxSize: Int): Gen[List[Enc]] = for {
    n <- Gen.choose(0,maxSize)
    xs <- Gen.listOfN(n, encryptedNumber)
  } yield xs

  def nonEmptyEncryptedList(maxSize: Int): Gen[List[Enc]] = for {
    n <- encryptedNumber
    ns <- encryptedList(maxSize-1)
  } yield n :: ns
}

trait CryptoCheck extends ScalaCheckGen {
  val keyRing = KeyRing.create
  val generators = EncryptedGens(keyRing)
}

