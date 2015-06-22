package crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import crypto.cipher._

case class EncryptedGens(keys: KeyRing) {
  val allowedChar: Gen[Char] = Gen.oneOf(OpeStr.ALLOWED_CHARS)

  val allowedString: Gen[String] =
    Gen.listOf(allowedChar).map(_.mkString).retryUntil(
      _.length <= keys.priv.opeStrPriv.maxLength)

  val encryptedString: Gen[EncString] = for {
    scheme <- Gen.oneOf("OPE","AES")
    s <- allowedString
  } yield (scheme match {
    case "OPE" => Common.encryptStrOpe(keys)(s)
    case "AES" => Common.encryptStrAes(keys)(s)
  })

  val encryptedStringSplit: Gen[(String,EncString)] = for {
    scheme <- Gen.oneOf("OPE","AES")
    s <- allowedString
    split <- Gen.oneOf(s).map("""\Q""" + _ + """\E""")
  } yield (split.toString,(scheme match {
    case "OPE" => Common.encryptStrOpe(keys)(s)
    case "AES" => Common.encryptStrAes(keys)(s)
  }))

  val allowedNumber: Gen[BigInt] =
    arbitrary[BigInt] retryUntil (keys.priv.opeIntPriv.domain.contains(_))

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
