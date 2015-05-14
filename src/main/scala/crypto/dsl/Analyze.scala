package crypto.dsl

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.list._

import crypto._
import crypto.cipher._

object Analysis {
  def requiredConversions[A](p: Crypto[A]): Int = {
    p.analyze(new (CryptoF ~> λ[α => Int]) {
      def apply[B](fa: CryptoF[B]): Int = fa match {
        case ToPaillier(PaillierEnc(_),_) => 0
        case ToPaillier(_,_) => 1
        case ToGamal(GamalEnc(_,_),_) => 0
        case ToGamal(_,_) => 1
        case ToAes(AesEnc(_),_) => 0
        case ToAes(_,_) => 1
        case ToOpe(OpeEnc(_),_) => 0
        case ToOpe(_,_) => 1

        case Plus(PaillierEnc(_),PaillierEnc(_),_) => 0
        case Plus(_,PaillierEnc(_),_) => 1
        case Plus(PaillierEnc(_),_,_) => 1
        case Plus(_,_,_) => 2

        case Mult(GamalEnc(_,_),GamalEnc(_,_),_) => 0
        case Mult(_,GamalEnc(_,_),_) => 1
        case Mult(GamalEnc(_,_),_,_) => 1
        case Mult(_,_,_) => 2

        case x => sys.error(s"don't know how many conversion for ${x}")
      }
    })
  }

  // eliminate all trivial conversions, e.g. toPaillier(PaillierEnc(_))
  def eliminateTrivalConversion[A](p: Crypto[A]): Crypto[A] = {
    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[A](fa: CryptoF[A]): Crypto[A] = {
        fa match {
          case ToPaillier(p@PaillierEnc(_),k) => FreeAp.point(k(p))
          case ToGamal(g@GamalEnc(_,_),k) => FreeAp.point(k(g))
          case Plus(x@PaillierEnc(_),y@PaillierEnc(_),k) => FreeAp.point(k(x+y))
          case Mult(x@GamalEnc(_,_),y@GamalEnc(_,_),k) => FreeAp.point(k(x*y))
          case x => FreeAp.lift(x)
        }
      }
    })
  }

  // perform all explicit conversions
  def preconvert[A](keyRing: KeyRing)(p: Crypto[A]): Crypto[A] = {
    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[A](fa: CryptoF[A]): Crypto[A] = {
        fa match {
          case ToPaillier(v,k) =>
            val r@PaillierEnc(_) = Common.convert(keyRing)(Additive, v)
            FreeAp.point(k(r))
          case ToGamal(v,k) =>
            val r@GamalEnc(_,_) = Common.convert(keyRing)(Multiplicative, v)
            FreeAp.point(k(r))
          case ToAes(v,k) =>
            val r@AesEnc(_) = Common.convert(keyRing)(Equality, v)
            FreeAp.point(k(r))
          case ToOpe(v,k) =>
            val r@OpeEnc(_) = Common.convert(keyRing)(Comparable, v)
            FreeAp.point(k(r))
          case x => FreeAp.lift(x)
        }
      }
    })
  }

  def extractNumbers[A](p: Crypto[A]): List[Enc] = {
    p.analyze(new (CryptoF ~> λ[α => List[Enc]]) {
      def apply[A](a: CryptoF[A]) = a match {
        case ToPaillier(v,k) => List(v)
        case ToGamal(v,k) => List(v)
        case ToAes(v,k) => List(v)
        case ToOpe(v,k) => List(v)

        case Mult(lhs,rhs,k) => List(lhs,rhs)
        case Plus(lhs,rhs,k) => List(lhs,rhs)
        case Equals(lhs,rhs,k) => List(lhs,rhs)
        case Compare(lhs,rhs,k) => List(lhs,rhs)

        case Sub(lhs,rhs,k) => List(lhs,rhs)
        case Div(lhs,rhs,k) => List(lhs,rhs)

        case Encrypt(s,v,k) => List()
        case Embed(p,k) => extractNumbers(p)
      }
    }).reverse
  }

  def replaceNumbers[A](p: Crypto[A])(subs: List[Enc]): Crypto[A] = {
    var cSubs = subs
    def takeHead(): Enc = {
      val h = cSubs.head
      cSubs = cSubs.tail
      h
    }

    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[A](fa: CryptoF[A]): Crypto[A] =
      fa match {
        case ToPaillier(v,k) =>
          val x = takeHead()
          FreeAp.lift(ToPaillier(x,k))
        case ToGamal(v,k) =>
          val x = takeHead()
          FreeAp.lift(ToGamal(x,k))
        case ToAes(v,k) =>
          val x = takeHead()
          FreeAp.lift(ToAes(x,k))
        case ToOpe(v,k) =>
          val x = takeHead()
          FreeAp.lift(ToOpe(x,k))
        case Mult(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Mult(l,r,k))
        case Plus(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Plus(l,r,k))
        case Equals(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Equals(l,r,k))
        case Compare(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Compare(l,r,k))
        case Sub(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Sub(l,r,k))
        case Div(lhs,rhs,k) =>
          val l = takeHead()
          val r = takeHead()
          FreeAp.lift(Div(l,r,k))

        case Encrypt(s,v,k) => FreeAp.lift(Encrypt(s,v,k))
        case Embed(p,k) => FreeAp.lift(Embed(p,k))
      }
    })
  }
}

object AnalysisMain extends App {
  val keyRing = KeyRing.create
  val interpreter = LocalInterpreter(keyRing)

  val xs = SampleData.fixed1.map(Common.encrypt(Multiplicative, keyRing)).take(200)

  val prog = sumA(Common.zero(keyRing))(xs)

  println(Common.decrypt(keyRing.priv)(interpreter.interpretA(prog)))

  println(s"Required conversions: ${Analysis.requiredConversions(prog)}")
  val nums = Analysis.extractNumbers(prog)
  println(s"Num extracted: ${nums.length}")
  println(s"Extracted numbers == original?: ${nums == xs}")

  val newXs = nums.map(Common.convert(keyRing)(Additive, _))

  println("Replacing...")
  val newProg = Analysis.replaceNumbers(prog)(newXs)
  println("Done replacing")
  val nums2 = Analysis.extractNumbers(newProg)
  println(s"Num extracted: ${nums2.length}")
  println(s"Required conversions: ${Analysis.requiredConversions(newProg)}")

  val r = interpreter.interpretA(newProg)
  println(Common.decrypt(keyRing.priv)(r))
}
