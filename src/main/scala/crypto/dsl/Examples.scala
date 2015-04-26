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
  import CryptoF.DSL._

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
  def sumList(xs: List[Enc]): Crypto[Enc] = xs.traverse(toPaillier).map(_.reduce(_+_))
  def multiplyList(xs: List[Enc]): Crypto[Enc] = xs.traverse(toGamal).map(_.reduce(_*_))
}

object SumExample extends App {
  import ExamplePrograms._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_))

  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.dec)

  val sumResult: Enc = locally.interpret {
    sum(encryptedList)
  }

  println(s"Result of sum without encryption: ${randomNumbers.sum mod keyRing.enc.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import ExamplePrograms._

  val randomNumbers = List.fill(5)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(NoEnc(_))

  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.dec)

  val productResult: Enc = locally.interpret {
    product(encryptedList)
  }

  println(s"Result of product without encryption: ${randomNumbers.product mod keyRing.enc.gamal.p}")
  println(s"Result of product with    encryption: ${decryption(productResult)}")
}
