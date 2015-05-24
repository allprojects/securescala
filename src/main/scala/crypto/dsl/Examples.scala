package crypto.dsl

import scala.language.higherKinds

import scalaz._
import scalaz.std.list._
import scalaz.std.map._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._

import scala.util.Random

import crypto._
import crypto.cipher._
import crypto.dsl.Implicits._

object ExamplePrograms {
  // https://en.wikipedia.org/wiki/Collatz_conjecture
  def collatzConjecture(n: Enc): CryptoM[Enc] = for {
    zero <- encrypt(Comparable)(0)
    xs <- List(1,2,3).traverse(encrypt(Multiplicative))
    r <- collatzConjectureHelper(zero,xs(0),xs(1),xs(2))(n)
  } yield r

  def collatzConjectureHelper(zero: Enc, one: Enc, two: Enc, three: Enc)(
    n: Enc): CryptoM[Enc] = for {
      greaterOne <- n > one
      r <- if (!greaterOne) n.lifted else for {
        cond <- isEven(n)
        r <- if (cond) {
          n / two >>= collatzConjectureHelper(zero,one,two,three)
        } else {
          three * n >>= (_+one) >>= collatzConjectureHelper(zero,one,two,three)
        }
      } yield r
  } yield r

  def fib(n: Enc): CryptoM[Enc] = for {
    (one,two) <- encrypt(Additive)(1) tuple encrypt(Additive)(2)
    r <- fibHelper(one,two)(n)
  } yield r

  def fibHelper(one: Enc, two: Enc)(n: Enc): CryptoM[Enc] = for {
    cmp <- n <= one
    r <- if (cmp) {
      one.lifted
    } else for {
      (n1,n2) <- (n-one) tuple (n-two)
      (f1,f2) <- fibHelper(one,two)(n1) tuple fibHelper(one,two)(n2)
      s <- f1 + f2
    } yield s
  } yield r

  def factorial(n: Enc): CryptoM[Enc] = for {
    (zero,one) <- encrypt(Multiplicative)(0) tuple (encrypt(Multiplicative)(1))
    r <- factorialHelper(zero, one)(n)
  } yield r

  def factorialHelper(zero: Enc, one: Enc)(n: Enc): CryptoM[Enc] = for {
    cond <- n =:= zero
    r <- if (cond) {
      one.lifted
    } else for {
      n1 <- n - one
      fact <- factorialHelper(zero,one)(n1)
      s <- n * fact
    } yield s
  } yield r

  def sumAndLength[F[_]:Traverse](z: PaillierEnc)(xs: F[Enc]): Crypto[(PaillierEnc,Enc)] =
    sumA(z)(xs).tuple(encrypt(Additive)(xs.length))

  // Requires zero to be passed in but uses applicative style and more specific
  // return type PaillierEnc
  def exampleMapSumValuesApplicative[A](zero: PaillierEnc)(
    input: Map[A,List[Enc]]): Crypto[Map[A,PaillierEnc]] = {

    input.traverse(xs => xs.sumOpt).map(_.mapValues(_.getOrElse(zero)))
  }

  // Does not require zero to be passed in but uses monadic style
  def exampleMapSumValuesMonad[A](input: Map[A,List[Enc]]): CryptoM[Map[A,Enc]] = for {
    maybeSums <- input.traverse(_.sumOpt)
    zero <- encrypt(Additive)(0)
  } yield maybeSums.mapValues(_.getOrElse(zero))

}

object Repl {
  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.priv)

  def runProgram[A](p: CryptoM[A]): A = locally.interpret(p)

  val zero = Common.zero(keyRing)
  val one = Common.one(keyRing)
}

object SumExample extends App {
 import Repl._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] =
    randomNumbers.map(Common.encrypt(Additive, keyRing))

  val sumResult = Repl.runProgram(sumA(zero)(encryptedList))

  println(s"Result of sum without encryption: ${randomNumbers.sum mod Repl.keyRing.pub.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs+1).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] =
    randomNumbers.map(Common.encrypt(Multiplicative, keyRing))

  val productResult: Enc = Repl.runProgram(productA(one)(encryptedList))

  println(s"Result of product without encryption: ${randomNumbers.product mod keyRing.pub.elgamal.p}")
  println(s"Result of product with    encryption: ${decryption(productResult)}")
}

object FactExample extends App {
  import ExamplePrograms._
  import Repl._

  val six = Common.encrypt(Multiplicative, keyRing)(6)
  val fortyTwo = Common.encrypt(Multiplicative, keyRing)(42)
  val result = Repl.runProgram(factorial(six))
  val result2 = Repl.runProgram(factorial(fortyTwo))

  println(s"Result of factorial(6) is ${decryption(result)}")
  println(s"Result of factorial(42) is ${decryption(result2)}")
}

object FibExample extends App {
  import ExamplePrograms._
  import Repl._

  val six = Common.encrypt(Multiplicative, keyRing)(6)
  val result = Repl.runProgram(fib(six))

  println(s"Result of fib(6) is ${decryption(result)}")
}

object AverageExample extends App {
  import ExamplePrograms._
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs)
  println(randomNumbers)

  val encNums: List[Enc] =
    randomNumbers.map(Common.encrypt(Additive, keyRing) compose BigInt.apply)

  val (sum,len) = Repl.runProgram(sumAndLength(zero)(encNums))
  val resultEnc = Common.decrypt(keyRing.priv)(sum) / Common.decrypt(keyRing.priv)(len)

  val normalAverage = randomNumbers.map(BigInt(_)).reduce(_+_) / randomNumbers.length
  println(s"Result for normal    program: ${normalAverage}")
  println(s"Result for encrypted program: ${resultEnc}")
}

object SampleData {
  val fixed1 = List[BigInt](
    388, 643, 458, 316, 742, 866, 829, 537, 895, 959, 527, 876, 758, 627, 841,
    745, 617, 298, 173, 641, 93, 179, 941, 891, 232, 352, 46, 665, 463, 260,
    130, 700, 604, 747, 675, 493, 648, 235, 211, 690, 166, 117, 216, 592, 735,
    656, 101, 651, 66, 962, 308, 576, 643, 700, 768, 210, 388, 505, 262, 313,
    322, 370, 475, 36, 870, 457, 456, 70, 636, 800, 3, 484, 560, 422, 80, 386,
    100, 757, 212, 669, 398, 843, 858, 312, 310, 834, 598, 598, 398, 454, 918,
    904, 866, 459, 273, 756, 774, 817, 327, 139, 663, 181, 968, 517, 329, 797,
    18, 235, 288, 887, 50, 297, 499, 22, 563, 670, 826, 34, 636, 209, 280, 764,
    227, 970, 819, 867, 535, 962, 312, 453, 580, 120, 972, 30, 824, 288, 54,
    894, 306, 690, 480, 898, 698, 37, 812, 513, 107, 790, 610, 361, 797, 977,
    407, 542, 224, 122, 210, 238, 786, 833, 417, 508, 857, 776, 648, 422, 741,
    378, 161, 933, 884, 337, 879, 386, 193, 204, 684, 784, 319, 451, 899, 641,
    79, 57, 744, 96, 569, 757, 875, 536, 270, 461, 331, 799, 724, 711, 463, 883,
    964, 205, 104, 752, 717, 373, 786, 210, 355, 370, 713, 773, 954, 250, 768,
    751, 412, 293, 429, 599, 570, 599, 681, 180, 47, 122, 323, 100, 248, 451,
    308, 624, 952, 551, 861, 120, 936, 809, 724, 936, 660, 491, 742, 646, 144,
    751, 486, 687, 721, 17, 700, 286, 552, 851, 240, 296, 745, 892, 77, 167,
    162, 998, 332, 360, 695, 250, 213, 755, 706, 611, 306, 55, 215, 251, 399,
    439, 358, 550, 481, 109, 650, 329, 983, 721, 78, 598, 158, 23, 439, 351,
    886, 874, 389, 981, 250, 233, 67, 116, 637, 116, 685, 116, 263, 778, 409,
    391, 808, 499, 375, 850, 563, 973, 82, 530, 275, 541, 296, 108, 820, 180,
    246, 816, 632, 4, 797, 163, 122, 765, 729, 976, 911, 136, 343, 211, 456,
    559, 550, 10, 428, 767, 255, 231, 659, 395, 911, 102, 269, 614, 435, 920,
    616, 572, 338, 642, 353, 534, 374, 357, 657, 281, 292, 122, 465, 330, 868,
    426, 753, 288, 714, 447, 17, 845, 434, 355, 27, 898, 75, 667, 479, 735, 642,
    247, 398, 772, 23, 777, 291, 238, 276, 351, 403, 545, 674, 159, 613, 6, 150,
    974, 533, 226, 513, 844, 530, 916, 517, 560, 455, 488, 499, 188, 351, 868,
    809, 677, 665, 302, 896, 89, 403, 80, 132, 716, 395, 321, 845, 793, 705,
    745, 147, 770, 120, 793, 142, 629, 275, 141, 547, 515, 948, 511, 170, 37,
    600, 201, 684, 965, 462, 247, 467, 462, 486, 375, 961, 265, 694, 101, 754,
    862, 640, 623, 70, 555, 23, 145, 926, 276, 768, 566, 696, 647, 974, 701,
    190, 18, 717, 284, 784, 372, 176, 555, 738, 444, 750, 943, 552, 810, 799,
    105, 421, 225, 517, 818, 307, 781, 678, 782, 565, 462, 653, 155, 177, 737
    )
}
