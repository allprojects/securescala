package dsl

import scalaz._
import Scalaz._
import scalaz.Free._
import scalaz.Ordering._
import scalaz.std.list

sealed trait Enc
// case class Paillier(self: BigInt) extends Enc
case class NoEnc(self: Int) extends Enc // No encryption for testing

sealed trait CryptoF[+K]
case class Mult[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]
case class Plus[K](lhs: Enc, rhs: Enc, k: Enc => K) extends CryptoF[K]
case class Encrypt[K](v: Int, k: Enc => K) extends CryptoF[K]
case class Compare[K](lhs: Enc, rhs: Enc, k: Ordering => K)  extends CryptoF[K]

object CryptoF {
  type Crypto[A] = Free[CryptoF, A]

  def multiply(lhs: Enc, rhs: Enc): Crypto[Enc] = liftF(Mult(lhs,rhs,identity))
  def add(lhs: Enc, rhs: Enc): Crypto[Enc] = liftF(Plus(lhs,rhs,identity))
  def encrypt(v: Int): Crypto[Enc] = liftF(Encrypt(v,identity))
  def compare(lhs: Enc, rhs: Enc): Crypto[Ordering] = liftF(Compare(lhs,rhs,identity))

  implicit val functor: Functor[CryptoF] = new Functor[CryptoF] {
    def map[A,B](fa: CryptoF[A])(f: A => B): CryptoF[B] = fa match {
      case Mult(lhs,rhs,k) => Mult(lhs,rhs,f compose k)
      case Plus(lhs,rhs,k) => Plus(lhs,rhs,f compose k)
      case Encrypt(v,k) => Encrypt(v,f compose k)
      case Compare(lhs,rhs,k) => Compare(lhs,rhs,f compose k)
    }
  }

  def interpret(p: Crypto[Enc]): Enc = p.resume match {
    case -\/(Mult(NoEnc(lhs),NoEnc(rhs),k)) =>
      println(s"Interpreting: ${lhs} * ${rhs}")
      interpret(k(NoEnc(lhs*rhs)))
    case -\/(Plus(NoEnc(lhs),NoEnc(rhs),k)) =>
      println(s"Interpreting: ${lhs} + ${rhs}")
      interpret(k(NoEnc(lhs+rhs)))
    case -\/(Compare(NoEnc(lhs),NoEnc(rhs),k)) =>
      println(s"Comparing: ${lhs} and ${rhs}")
      interpret(k(lhs ?|? rhs))
    case -\/(Encrypt(v,k)) =>
      println(s"Encrypting: ${v}")
      interpret(k(NoEnc(v)))
    case \/-(x) =>
      println(s"Final result: ${x}")
      x
  }
}

object ExamplePrograms {
  import CryptoF._

  def encMax(a: Enc, b: Enc) = for {
    comparison <- compare(a,b)
    result <- comparison match {
      case LT => Free.point[CryptoF, Enc](b)
      case _ => Free.point[CryptoF, Enc](a)
    }
  } yield result

  def sum(xs: List[Enc]): Crypto[Enc] = for {
    init <- encrypt(0)
    r <- xs.foldLeftM(init) { (accum,x) => add(accum,x) }
  } yield r

  def product(xs: List[Enc]): Crypto[Enc] = for {
    init <- encrypt(1)
    r <- xs.foldLeftM(init) { (accum,x) => multiply(accum,x) }
  } yield r

  def prog1(a: Enc, b: Enc) = for {
    aTimesB <- multiply(a,b)
    c <- encrypt(32)
    r <- add(aTimesB, c)
  } yield r
}

object Main extends App {
  import ExamplePrograms._
  import CryptoF._

  println("Summing a list")
  def encrypt: Int => Enc = NoEnc(_) // stub

  val encryptedList = List.iterate(1,10)(_ + 1).map(encrypt)

  interpret(sum(encryptedList))
  interpret(product(encryptedList))
}
