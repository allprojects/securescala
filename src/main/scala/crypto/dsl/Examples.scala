package crypto.dsl

import scala.language.higherKinds

import scalaz._
import Scalaz._
import scalaz.Ordering._
import scalaz.std.list

import scala.util.Random

import crypto.cipher._
import crypto.KeyRing

object ExamplePrograms {

  def encMax(a: Enc, b: Enc) = for {
    comparison <- compare(a,b).monadic
    result <- comparison match {
      case LT => Free.point[CryptoF, Enc](b)
      case _ => Free.point[CryptoF, Enc](a)
    }
  } yield result

  // General case for every foldable, but needs monadic interface
  def sum[F[_]:Foldable](xs: F[Enc]): CryptoM[Enc] = for {
    init <- encrypt(0).monadic
    r <- xs.foldLeftM[CryptoM,Enc](init) { (accum,x) => add(accum,x).monadic}
  } yield r

  def product(xs: List[Enc]): CryptoM[Enc] = for {
    init <- encrypt(1).monadic
    r <- xs.foldLeftM[CryptoM,Enc](init) { (accum,x) => multiply(accum,x).monadic }
  } yield r

  def prog1(a: Enc, b: Enc) = for {
    aTimesB <- multiply(a,b).monadic
    c <- encrypt(32).monadic
    r <- add(aTimesB, c).monadic
  } yield r

  // Use applicative, allows for batch encrypting/decrypting
  def sumList(xs: List[Enc]): Crypto[PaillierEnc] =
    xs.traverse(toPaillier).map(_.reduce(_+_))
  def multiplyList(xs: List[Enc]): Crypto[Enc] = xs.traverse(toGamal).map(_.reduce(_*_))

  def sumAndProduct(xs: List[Enc]): Crypto[(Enc,Enc)] = {
    val theSum = sumList(xs)
    val theProduct = multiplyList(xs)
    (theSum |@| theProduct) { (x,y) => (x,y) }
  }

  def factorial(n: Enc): CryptoM[Enc] = for {
    zero <- encrypt(0).monadic
    r <- equal(n,zero).monadic.ifM(encrypt(1).monadic,
      for {
        one <- encrypt(1).monadic
        newN <- subtract(n,one).monadic
        intermediateR <- factorial(newN)
        result <- multiply(n,intermediateR).monadic
      } yield result)
  } yield r
}

object REPL {

  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.dec)

  def runProgram[A](p: CryptoM[A]): A = locally.interpret(p)
  def encrypt(i: Int): Enc = Common.encrypt(Additive, keyRing.enc)(i)
}

object SumExample extends App {
  import ExamplePrograms._
  import REPL._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_)) // TODO stub for encryption used

  val sumResult = REPL.runProgram(sum(encryptedList))

  println(s"Result of sum without encryption: ${randomNumbers.sum mod REPL.keyRing.enc.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import ExamplePrograms._
  import REPL._

  val randomNumbers = List.fill(5)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_))

  val productResult: Enc = REPL.runProgram(product(encryptedList))

  println(s"Result of product without encryption: ${randomNumbers.product mod keyRing.enc.gamal.p}")
  println(s"Result of product with    encryption: ${decryption(productResult)}")
}

object FactExample extends App {
  import ExamplePrograms._
  import REPL._

  val six = REPL.encrypt(6)
  val result = REPL.runProgram(factorial(six))

  println(s"Result of factorial(6) is ${decryption(result)}")
}
