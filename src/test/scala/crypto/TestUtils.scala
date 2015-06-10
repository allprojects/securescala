package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

case class EncryptedGens(keys: KeyRing) {
  val allowedNumber = arbitrary[BigInt] retryUntil (keys.priv.opeIntPriv.domain.contains(_))

  def encryptedNumber: Gen[EncInt] = for {
    scheme <- Gen.oneOf(Additive, Multiplicative, Equality, Comparable)
    i <- allowedNumber
  } yield Common.encrypt(scheme, keys)(i)

  def encryptedList(maxSize: Int): Gen[List[EncInt]] = for {
    n <- Gen.choose(0,maxSize)
    xs <- Gen.listOfN(n, encryptedNumber)
  } yield xs

  def nonEmptyEncryptedList(maxSize: Int): Gen[List[EncInt]] = for {
    n <- encryptedNumber
    ns <- encryptedList(maxSize-1)
  } yield n :: ns
}

trait CryptoCheck {
  val keyRing = KeyRing.create
  val generators = EncryptedGens(keyRing)
}

