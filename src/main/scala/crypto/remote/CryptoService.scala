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

  def convert(s: Scheme)(in: Enc): Future[Enc]
  def batchConvert(xs: List[(Scheme,Enc)]): Future[List[Enc]]

  def encrypt(s: Scheme)(in: Int): Future[Enc]
  def batchEncrypt(xs: List[(Scheme,Int)]): Future[List[Enc]]

  def decryptAndPrint(v: Enc): Unit
}

trait CryptoServicePlus extends CryptoService {
  def subtract(lhs: Enc, rhs: Enc): Future[Enc]
  def divide(lhs: Enc, rhs: Enc): Future[Enc]
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

  override def divide(lhs: Enc, rhs: Enc): Future[Enc] = Future.successful {
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
