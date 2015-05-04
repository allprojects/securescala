package crypto.dsl

import scala.language.higherKinds

import scalaz._
import Scalaz._
import scalaz.std.list

import scala.util.Random

import crypto._
import crypto.cipher._

object ExamplePrograms {
  def factorial(n: Enc): CryptoM[Enc] = for {
    zero <- encrypt(0)
    r <- embed(equal(n,zero)).ifM(encrypt(1),
      for {
        one <- encrypt(1)
        newN <- subtract(n,one)
        intermediateR <- factorial(newN)
        result <- multiply(n,intermediateR)
      } yield result)
  } yield r

  def sumAndLength[F[_]:Traverse](zero: PaillierEnc)(xs: F[Enc]): CryptoM[(Enc,Enc)] =
    embed { (sumA(zero)(xs) |@| encrypt(xs.length))((x,y) => (x,y))}
}

object Repl {
  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.priv)

  def runProgram[A](p: CryptoM[A]): A = locally.interpret(p)

  val zero@PaillierEnc(_) = Common.encrypt(Additive, keyRing.pub)(0)
  val one@GamalEnc(_,_) = Common.encrypt(Multiplicative, keyRing.pub)(1)
}

object SumExample extends App {
 import Repl._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(Common.encrypt(Additive, keyRing.pub))

  val sumResult = Repl.runProgram(sumA(zero)(encryptedList))

  println(s"Result of sum without encryption: ${randomNumbers.sum mod Repl.keyRing.pub.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs+1).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(Common.encrypt(Multiplicative, keyRing.pub))

  val productResult: Enc = Repl.runProgram(productA(one)(encryptedList))

  println(s"Result of product without encryption: ${randomNumbers.product mod keyRing.pub.gamal.p}")
  println(s"Result of product with    encryption: ${decryption(productResult)}")
}

object FactExample extends App {
  import ExamplePrograms._
  import Repl._

  val six = Common.encrypt(Multiplicative, keyRing.pub)(6)
  val result = Repl.runProgram(factorial(six))

  println(s"Result of factorial(6) is ${decryption(result)}")
}

object AverageExample extends App {
  import ExamplePrograms._
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs)
  println(randomNumbers)

  val encNums = randomNumbers.map(Common.encrypt(Additive, keyRing.pub) compose BigInt.apply)

  val (sum,len) = Repl.runProgram(sumAndLength(zero)(encNums))
  val resultEnc = Common.decrypt(keyRing.priv)(sum) / Common.decrypt(keyRing.priv)(len)

  val normalAverage = randomNumbers.map(BigInt(_)).reduce(_+_) / randomNumbers.length
  println(s"Result for normal    program: ${normalAverage}")
  println(s"Result for encrypted program: ${resultEnc}")
}
