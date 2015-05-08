package crypto.dsl

import scala.language.higherKinds

import scalaz._
import Scalaz._
import scalaz.std.list

import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration.Duration

import crypto._
import crypto.cipher._

object ExamplePrograms {
  def factorial(n: Enc): CryptoM[Enc] = for {
    zero <- encrypt(Multiplicative)(0)
    r <- embed(equal(n,zero)).ifM(encrypt(Multiplicative)(1),
      for {
        one <- encrypt(Multiplicative)(1)
        newN <- subtract(n,one)
        intermediateR <- factorial(newN)
        result <- multiply(n,intermediateR)
      } yield result)
  } yield r

  def sumAndLength[F[_]:Traverse](zero: PaillierEnc)(xs: F[Enc]): CryptoM[(Enc,Enc)] =
    embed { (sumA(zero)(xs) |@| encrypt(Additive)(xs.length))((x,y) => (x,y))}
}

object Repl {
  val keyRing = KeyRing.create

  val locally = LocalInterpreter(keyRing)
  val decryption = Common.decrypt(keyRing.priv)

  def runProgram[A](p: CryptoM[A]): A = locally.interpret(p)

  val zero@PaillierEnc(_) = Common.encryptPub(Additive, keyRing.pub)(0)
  val one@GamalEnc(_,_) = Common.encryptPub(Multiplicative, keyRing.pub)(1)
}

object SumExample extends App {
 import Repl._

  val randomNumbers = List.fill(20)(Random.nextInt.abs).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(Common.encryptPub(Additive, keyRing.pub))

  val sumResult = Repl.runProgram(sumA(zero)(encryptedList))

  println(s"Result of sum without encryption: ${randomNumbers.sum mod Repl.keyRing.pub.paillier.n}")
  println(s"Result of sum with    encryption: ${decryption(sumResult)}")
}

object MultExample extends App {
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs+1).map(BigInt(_))
  println(s"Sequence of random numbers: ${randomNumbers}")

  val encryptedList: List[Enc] = randomNumbers.map(Common.encryptPub(Multiplicative, keyRing.pub))

  val productResult: Enc = Repl.runProgram(productA(one)(encryptedList))

  println(s"Result of product without encryption: ${randomNumbers.product mod keyRing.pub.gamal.p}")
  println(s"Result of product with    encryption: ${decryption(productResult)}")
}

object FactExample extends App {
  import ExamplePrograms._
  import Repl._

  val six = Common.encryptPub(Multiplicative, keyRing.pub)(6)
  val result = Repl.runProgram(factorial(six))

  println(s"Result of factorial(6) is ${decryption(result)}")
}

object AverageExample extends App {
  import ExamplePrograms._
  import Repl._

  val randomNumbers = List.fill(5)(Random.nextInt.abs)
  println(randomNumbers)

  val encNums = randomNumbers.map(Common.encryptPub(Additive, keyRing.pub) compose BigInt.apply)

  val (sum,len) = Repl.runProgram(sumAndLength(zero)(encNums))
  val resultEnc = Common.decrypt(keyRing.priv)(sum) / Common.decrypt(keyRing.priv)(len)

  val normalAverage = randomNumbers.map(BigInt(_)).reduce(_+_) / randomNumbers.length
  println(s"Result for normal    program: ${normalAverage}")
  println(s"Result for encrypted program: ${resultEnc}")
}

object ParallelExample extends App {
  import Repl._
  import scala.concurrent.ExecutionContext.Implicits.global

  // List.fill(500)(Random.nextInt(1000).abs).map(BigInt(_))
  val encryptedList: List[Enc] = SampleData.fixed1.map(Common.encryptPub(Multiplicative, keyRing.pub))

  // parallel version
  val startPar = System.currentTimeMillis
  val sumResult = Repl.locally.interpPar(sumA(zero)(encryptedList))
  val finalResult = sumResult.map(decryption)
  val rPar = Await.result(finalResult, Duration.Inf)
  val endPar = System.currentTimeMillis
  println(s"Parallel result:   $rPar")

  //sequential
  val startSeq = System.currentTimeMillis
  val rSeq = decryption(Repl.runProgram(sumA(zero)(encryptedList)))
  val endSeq = System.currentTimeMillis
  println(s"Sequential result: $rSeq")

  println(s"Parallel time:   ${endPar-startPar}")
  println(s"Sequential time: ${endSeq-startSeq}")
  println(s"Speedup:         ${(endSeq-startSeq).toFloat/(endPar-startPar)}")
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
