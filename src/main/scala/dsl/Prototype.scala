package dsl

import scalaz._
import scalaz.Free._

sealed trait Enc {
  def +(that: Enc) = this match {
    case Unencrypted(n) => that match {
      case Unencrypted(m) => Unencrypted(n + m)
    }
  }

  def *(that: Enc) = this match {
    case Unencrypted(n) => that match {
      case Unencrypted(m) => Unencrypted(n * m)
    }
  }
}
// case class Paillier(self: BigInt) extends Enc
case class Unencrypted(self: Int) extends Enc

sealed trait CryptoF[K]
case class Mult[K](lhs: Enc, rhs: Enc, K: Enc => K) extends CryptoF[K]
case class Plus[K](lhs: Enc, rhs: Enc, K: Enc => K) extends CryptoF[K]
case class Encrypt[K](v: Int, K: Enc => K) extends CryptoF[K]

object CryptoF {
  type Crypto[A] = Free[CryptoF, A]

  def multiply(lhs: Enc, rhs: Enc): Crypto[Enc] = liftF(Mult(lhs,rhs,identity))
  def add(lhs: Enc, rhs: Enc): Crypto[Enc] = liftF(Plus(lhs,rhs,identity))
  def encrypt(v: Int): Crypto[Enc] = liftF(Encrypt(v,identity))

  implicit val functor: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case Mult(lhs,rhs,k) => Mult(lhs,rhs,f compose k)
      case Plus(lhs,rhs,k) => Plus(lhs,rhs,f compose k)
      case Encrypt(v,k) => Encrypt(v,f compose k)
    }
  }

  def locally(p: Crypto[Enc]): Enc = p.resume match {
    case -\/(Mult(lhs,rhs,k)) =>
      println("Multiplication")
      locally(k(lhs*rhs))
    case -\/(Plus(lhs,rhs,k)) =>
      println("Addition")
      locally(k(lhs+rhs))
    case -\/(Encrypt(v,k)) =>
      locally(k(Unencrypted(v)))
    case \/-(x) => x
  }
}

object Example extends App {
  import CryptoF._

  def prog1(a: Enc, b: Enc) = for {
    aTimesB <- multiply(a,b)
    c <- encrypt(42)
    r <- add(aTimesB, c)
  } yield r

  val myprog = prog1(Unencrypted(5), Unencrypted(2))
  val Unencrypted(result) = locally(myprog)
  println("Final result: " + result)
}
