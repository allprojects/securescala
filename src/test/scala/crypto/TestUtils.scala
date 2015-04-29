package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

object TestUtils {
  val positiveInt = arbitrary[BigInt] retryUntil (_.signum == 1)

  def encryptedNumber(keys: EncKeys): Gen[Enc] = for {
    scheme <- Gen.oneOf(Additive, Multiplicative) // TODO add other schemes
    i <- positiveInt
  } yield Common.encrypt(scheme, keys)(i)

  def encryptedList(keys: EncKeys): Gen[List[Enc]] = for {
    n <- Gen.choose(0,10)
    xs <- Gen.listOfN(n, encryptedNumber(keys))
  } yield xs

  def nonEmptyEncryptedList(keys: EncKeys): Gen[List[Enc]] = for {
    n <- encryptedNumber(keys)
    ns <- encryptedList(keys)
  } yield n :: ns
}
