package crypto.cipher

import argonaut._
import Argonaut._

import java.security.SecureRandom
import scala.collection.immutable.NumericRange
import scala.util.Try
import scalaz._
import scalaz.std.list._
import scalaz.std.stream
import scalaz.syntax.traverse._

class OpeNative { // `class` required for javah/native interface
  @native def nativeEncrypt(
    password: String, plaintext: String, plainTextBits: Int, cipherTextBits: Int): String

  @native def nativeDecrypt(
    password: String, ciphertext: String, plainTextBits: Int, cipherTextBits: Int): String
}

object OpeNative {
  val home = System.getProperty("user.home")
  System.load(home + "/libope.so")
  private val instance = new OpeNative

  def encrypt(
    password: String, plaintext: String, plainTextBits: Int, cipherTextBits: Int): String =
    this.synchronized(
      instance.nativeEncrypt(password,plaintext,plainTextBits,cipherTextBits))

  def decrypt(
    password: String,ciphertext: String,plainTextBits: Int, cipherTextBits: Int): String =
    this.synchronized(
      instance.nativeDecrypt(password,ciphertext,plainTextBits,cipherTextBits))
}

object OpeInt {
  private val numPlainTextBits = 64
  private val numCipherTextBits = 96

  case class Encryptor(f: BigInt => (String \/ BigInt))
      extends (BigInt => String \/ BigInt) {
    def apply(x: BigInt) = f(x)
  }

  case class Decryptor(f: BigInt => BigInt) extends (BigInt => BigInt) {
    def apply(x: BigInt) = f(x)
  }

  case class PrivKey(
    key: String,
    bits: Int,
    plainBits: Int,
    cipherBits: Int,
    domain: CipherDomain[BigInt]
  )

  object PrivKey {
    implicit def codec = casecodec5(PrivKey.apply,PrivKey.unapply)(
      "key","bits","plainBits","cipherBits","domain")
  }

  def create(bits: Int): (Encryptor, Decryptor, PrivKey) = {
    val key = generateKey(bits, numPlainTextBits, numCipherTextBits)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)), key)
  }

  def fromKey(key: PrivKey): (Encryptor, Decryptor) = {
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)))
  }

  private def generateKey(bits: Int, plainBits: Int, cipherBits: Int): PrivKey = {
    val limit = BigInt(2).pow(plainBits) / 2
    val dom = CipherDomain(-limit,limit-1)
    PrivKey(BigInt(bits, new SecureRandom).toString(32), bits, plainBits, cipherBits, dom)
  }

  def encrypt(priv: PrivKey)(input: BigInt): String \/ BigInt = input match {
    case x if !priv.domain.contains(x) => -\/("OPE: Input out of range")
    case x => \/.fromTryCatchNonFatal {
      OpeNative.encrypt(
        priv.key,
        (input+priv.domain.max+1).toString,
        priv.plainBits,
        priv.cipherBits)
    }.leftMap(e => "OPE: " + e.getMessage).map(BigInt(_))
  }

  def decrypt(priv: PrivKey)(input: BigInt): BigInt = {
    val plain = BigInt(
      OpeNative.decrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits))

    plain - (priv.domain.max+1)
  }
}

object OpeStr {
  val ALLOWED_CHARS: Seq[Char] = {
    val nums = "0123456789"
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    val special = " -.()[],\"\n:;'=`$<>+$*_!?"

    Seq(nums ++ chars ++ special: _*).distinct.sortBy(_.toByte)
  }

  private val CHARSET_BASE: Int = ALLOWED_CHARS.length + 1
  private val BITS_PER_CHAR: Int =
    math.ceil(math.log((CHARSET_BASE+1).toDouble)/math.log(2)).toInt
  private val ADDITIONAL_CIPHERTEXT_BITS = 16

  case class Encryptor(f: String => (String \/ BigInt))
      extends (String => String \/ BigInt) {
    def apply(x: String) = f(x)
  }

  case class Decryptor(f: BigInt => String \/ String)
      extends (BigInt => String \/ String) {
    def apply(x: BigInt) = f(x)
  }

  case class PrivKey(
    key: String,
    bits: Int,
    plainBits: Int,
    cipherBits: Int,
    domain: CipherDomain[BigInt],
    maxLength: Int
  )

  object PrivKey {
    implicit def codec = casecodec6(PrivKey.apply,PrivKey.unapply)(
      "key","bits","plainBits","cipherBits","domain","maxLength")
  }

  def create(bits: Int, maxLength: Int): (Encryptor, Decryptor, PrivKey) = {
    val plainTextBits = maxLength * BITS_PER_CHAR
    val cipherTextBits = plainTextBits + ADDITIONAL_CIPHERTEXT_BITS
    val key = generateKey(bits, plainTextBits, cipherTextBits, maxLength)
    (Encryptor(encrypt(key)), Decryptor(decrypt(key)), key)
  }

  def fromKey(key: PrivKey) = (Encryptor(encrypt(key)), Decryptor(decrypt(key)))

  private def generateKey(
    bits: Int, plainBits: Int, cipherBits: Int, maxLength: Int): PrivKey = {

    val limit = BigInt(2).pow(plainBits) / 2
    val dom = CipherDomain(-limit,limit-1)
    PrivKey(
      BigInt(bits, new SecureRandom).toString(32),
      bits,
      plainBits,
      cipherBits,
      dom,
      maxLength
    )
  }

  def encrypt(priv: PrivKey)(input: String): String \/ BigInt = {
    plainToNumeric(priv)(input).map { numInput =>
      BigInt(OpeNative.encrypt(priv.key,numInput,priv.plainBits,priv.cipherBits))
    }
  }

  def encryptChunk(priv: PrivKey)(input: String): String \/ List[BigInt] = {
    input.
      grouped(priv.maxLength).
      toList.
      traverseU(encrypt(priv))
  }

  def decrypt(priv: PrivKey)(input: BigInt): String \/ String = {
    val numeric =
      OpeNative.decrypt(priv.key, input.toString, priv.plainBits, priv.cipherBits)

    numericToPlain(priv)(numeric)
  }

  def decryptChunk(priv: PrivKey)(input: List[BigInt]): String \/ String = {
    input.traverseU(decrypt(priv)).map(_.mkString)
  }

  def plainToNumeric(priv: PrivKey)(plaintext: String): String \/ String = {
    if (plaintext.length > priv.maxLength) {
      -\/(
        s"Max allowed length is ${priv.maxLength} given: ${plaintext.length} ($plaintext)")
    } else {
      val charIndices: String \/ List[Int] =
        plaintext.toList.traverseU { char =>
          val index = ALLOWED_CHARS.indexOf(char)
          if (index != -1) Success(index) else Failure(DList(char))
        }.disjunction.leftMap(_.toSet.mkString("Invalid chars found: ",",",""))

      charIndices.map(_.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (charIndex,i)) =>
        acc + (BigInt(charIndex + 1) * BigInt(CHARSET_BASE).pow(priv.maxLength-i-1))
      }.toString)
    }
  }

  def numericToPlain(priv: PrivKey)(numeric: String): String \/ String = {
    val numericInt = BigInt(numeric)

    def findPower(power: Int): Int =
      if (numericInt / BigInt(CHARSET_BASE).pow(power+1) > 0) findPower(power+1) else power
    val power = findPower(0)

    \/-((0 to power).reverse.foldLeft((numericInt,"")) { case ((numericInt_,plain),i) =>
      val basePow = BigInt(CHARSET_BASE).pow(i)
      val charIndex = (numericInt_ / basePow) - 1
      if (charIndex == -1) {
        (numericInt_,plain)
      } else {
        (numericInt_.mod(basePow),plain + ALLOWED_CHARS(charIndex.toInt))
      }
    }._2)
  }
}
