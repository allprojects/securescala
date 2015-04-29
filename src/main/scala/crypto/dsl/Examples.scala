package crypto.dsl

import scala.language.higherKinds

import scalaz._
import Scalaz._
import scalaz.std.list

import scala.util.Random

import crypto.cipher._
import crypto.KeyRing

object ExamplePrograms {
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

  val zero@PaillierEnc(_) = Common.encrypt(Additive, keyRing.enc)(0)
  val one@GamalEnc(_,_) = Common.encrypt(Multiplicative, keyRing.enc)(0)
}

object SumExample extends App {
  import REPL._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_)) // TODO stub for encryption used

  val sumResult = REPL.runProgram(sumA(zero)(encryptedList).monadic)

  println(s"Result of sum without encryption: ${randomNumbers.sum mod REPL.keyRing.enc.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import REPL._

  val randomNumbers = List.fill(5)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_))

  val productResult: Enc = REPL.runProgram(productA(one)(encryptedList).monadic)

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
