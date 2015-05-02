package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

object TestUtils {
  val positiveInt = arbitrary[BigInt] retryUntil (_.signum == 1)

  def encryptedNumber(keys: PubKeys): Gen[Enc] = for {
    scheme <- Gen.oneOf(Additive, Multiplicative) // TODO add other schemes
    i <- positiveInt
  } yield Common.encrypt(scheme, keys)(i)

  def encryptedList(maxSize: Int)(keys: PubKeys): Gen[List[Enc]] = for {
    n <- Gen.choose(0,maxSize)
    xs <- Gen.listOfN(n, encryptedNumber(keys))
  } yield xs

  def nonEmptyEncryptedList(maxSize: Int)(keys: PubKeys): Gen[List[Enc]] = for {
    n <- encryptedNumber(keys)
    ns <- encryptedList(maxSize-1)(keys)
  } yield n :: ns
}

