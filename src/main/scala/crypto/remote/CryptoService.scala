package crypto.remote

import scala.concurrent._

import crypto._
import crypto.cipher._

trait CryptoService {
  /** Replies with the public keys of the KeyRing used by this service */
  def publicKeys: Future[PubKeys]

  def toPaillier(in: Enc): Future[PaillierEnc]
  def toElGamal(in: Enc): Future[ElGamalEnc]
  def toAes(in: Enc): Future[AesEnc]
  def toOpe(in: Enc): Future[OpeEnc]

  /** Convert the encoded value for the given scheme */
  def convert(s: Scheme)(in: Enc): Future[Enc]
  /** Process the list of (scheme,encoding) and convert the encoding to
    * scheme (if necessary) before replying with the whole list of
    * results
    */
  def batchConvert(xs: List[(Scheme,Enc)]): Future[List[Enc]]

  /** Encrypt the plain number with the given scheme NOTE: the value is
    * NOT encrypted for sending and therefore may be visible to
    * others!  If possible you should use the public keys and encrypt
    * them with an asymmetric scheme like paillier or elgamal before
    * sending it.
    */
  def encrypt(s: Scheme)(in: Int): Future[Enc]
  /** Process the list of (scheme,integer) and encrypt them, reply after
    * the whole list is processed
    */
  def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[Enc]]

  /** Decrypt the value and print it locally (where the service runs) to stdout */
  def decryptAndPrint(v: Enc): Unit
}

trait CryptoServicePlus extends CryptoService {
  def subtract(lhs: Enc, rhs: Enc): Future[Enc]
  def integerDivide(lhs: Enc, rhs: Enc): Future[Enc]
}

class CryptoServiceImpl(keyRing: KeyRing) extends CryptoService with CryptoServicePlus {
  private def doConvert(s: Scheme, in: Enc) = Common.depConvert(keyRing)(s,in)
  private def additive(x: Enc): PaillierEnc = doConvert(Additive, x)
  private def multiplicative(x: Enc): ElGamalEnc = doConvert(Multiplicative, x)
  private def equality(x: Enc): AesEnc = doConvert(Equality, x)
  private def comparable(x: Enc): OpeEnc = doConvert(Comparable, x)

  override def publicKeys = Future.successful(keyRing.pub)

  override def toPaillier(in: Enc): Future[PaillierEnc] = Future.successful(additive(in))
  override def toElGamal(in: Enc): Future[ElGamalEnc] =
    Future.successful(multiplicative(in))
  override def toAes(in: Enc): Future[AesEnc] = Future.successful(equality(in))
  override def toOpe(in: Enc): Future[OpeEnc] = Future.successful(comparable(in))

  override def convert(s: Scheme)(in: Enc): Future[Enc] =
    Future.successful(Common.convert(keyRing)(s,in))
  override def batchConvert(xs: List[(Scheme,Enc)]): Future[List[Enc]] =
    Future.successful(xs.map { case (s, enc) => Common.convert(keyRing)(s,enc)})

  override def encrypt(s: Scheme)(in: Int): Future[Enc] =
    Future.successful(Common.encrypt(s, keyRing)(in))
  override def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[Enc]] =
    Future.successful(xs.map { case (s, i) => Common.encrypt(s, keyRing)(i)})

  override def decryptAndPrint(v: Enc): Unit = println(Common.decrypt(keyRing.priv)(v))

  override def integerDivide(lhs: Enc, rhs: Enc): Future[Enc] = Future.successful {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs / plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }

  override def subtract(lhs: Enc, rhs: Enc): Future[Enc] = Future.successful {
    val plainLhs = Common.decrypt(keyRing.priv)(lhs)
    val plainRhs = Common.decrypt(keyRing.priv)(rhs)
    val result = plainLhs - plainRhs
    Common.encrypt(Additive, keyRing)(result)
  }

}
