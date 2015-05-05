package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

object TestUtils {
  val posInt = arbitrary[BigInt] retryUntil (_.signum == 1)

  def opeAllowedInts(key: Ope.PrivKey) =
    posInt retryUntil (_.bitLength <= key.plainBits)

  def encryptedNumber(keyRing: KeyRing)(g: Gen[BigInt]): Gen[Enc] = for {
    scheme <- Gen.oneOf(Additive, Multiplicative, Equality, Comparable)
    i <- g
  } yield 
    scheme match {
      case Additive => Common.encryptPub(Additive, keyRing.pub)(i)
      case Multiplicative => Common.encryptPub(Multiplicative, keyRing.pub)(i)
      case Equality => AesEnc(keyRing.priv.aesEnc(i.toByteArray))
      case Comparable => OpeEnc(keyRing.priv.opeIntEnc(i))
    }

  def encryptedList(maxSize: Int)(keys: KeyRing): Gen[List[Enc]] = for {
    n <- Gen.choose(0,maxSize)
    xs <- Gen.listOfN(n, encryptedNumber(keys)(posInt))
  } yield xs

  def nonEmptyEncryptedList(maxSize: Int)(keys: KeyRing): Gen[List[Enc]] = for {
    n <- encryptedNumber(keys)(posInt)
    ns <- encryptedList(maxSize-1)(keys)
  } yield n :: ns
}

