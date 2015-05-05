package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

object TestUtils {
  val positiveInt = arbitrary[BigInt] retryUntil (_.signum == 1)

  def encryptedNumber(keyRing: KeyRing): Gen[Enc] = for {
    scheme <- Gen.oneOf("Paillier","ElGamal","AES","OPE")
    i <- positiveInt
  } yield 
    scheme match {
      case "Paillier" => Common.encrypt(Additive, keyRing.pub)(i)
      case "ElGamal" => Common.encrypt(Multiplicative, keyRing.pub)(i)
      case "AES" => AesEnc(keyRing.priv.aesEnc(i.toByteArray))
      case "OPE" => OpeEnc(keyRing.priv.opeIntEnc(i))
      case x => sys.error(s"Illegal scheme: '$x'")
    }

  def encryptedList(maxSize: Int)(keys: KeyRing): Gen[List[Enc]] = for {
    n <- Gen.choose(0,maxSize)
    xs <- Gen.listOfN(n, encryptedNumber(keys))
  } yield xs

  def nonEmptyEncryptedList(maxSize: Int)(keys: KeyRing): Gen[List[Enc]] = for {
    n <- encryptedNumber(keys)
    ns <- encryptedList(maxSize-1)(keys)
  } yield n :: ns
}

