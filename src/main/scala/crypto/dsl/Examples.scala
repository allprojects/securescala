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
    comparison <- compare(a,b)
    result <- comparison match {
      case LT => Free.point[CryptoF, Enc](b)
      case _ => Free.point[CryptoF, Enc](a)
    }
  } yield result

  def sum[F[_]:Foldable](xs: F[Enc]): CryptoM[Enc] = for {
    init <- encrypt(0)
    r <- xs.foldLeftM(init) { (accum,x) => add(accum,x) }
  } yield r

  def product(xs: List[Enc]): CryptoM[Enc] = for {
    init <- encrypt(1)
    r <- xs.foldLeftM(init) { (accum,x) => multiply(accum,x) }
  } yield r

  def prog1(a: Enc, b: Enc) = for {
    aTimesB <- multiply(a,b)
    c <- encrypt(32)
    r <- add(aTimesB, c)
  } yield r
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
