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
        case ToGamal(ElGamalEnc(_,_),_) => 0
        case ToGamal(_,_) => 1
        case ToAes(AesEnc(_),_) => 0
        case ToAes(_,_) => 1
        case ToOpe(OpeEnc(_),_) => 0
        case ToOpe(_,_) => 1

        case Plus(PaillierEnc(_),PaillierEnc(_),_) => 0
        case Plus(_,PaillierEnc(_),_) => 1
        case Plus(PaillierEnc(_),_,_) => 1
        case Plus(_,_,_) => 2

        case Mult(ElGamalEnc(_,_),ElGamalEnc(_,_),_) => 0
        case Mult(_,ElGamalEnc(_,_),_) => 1
        case Mult(ElGamalEnc(_,_),_,_) => 1
        case Mult(_,_,_) => 2

        case x => sys.error(s"don't know how many conversion for ${x}")
      }
    })
  }

  // eliminate all trivial conversions, e.g. toPaillier(PaillierEnc(_))
  def eliminateTrivalConversion[A](p: Crypto[A]): Crypto[A] = {
    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[B](fa: CryptoF[B]): Crypto[B] = {
        fa match {
          case ToPaillier(p@PaillierEnc(_),k) => FreeAp.point(k(p))
          case ToGamal(g@ElGamalEnc(_,_),k) => FreeAp.point(k(g))
          case Plus(x@PaillierEnc(_),y@PaillierEnc(_),k) => FreeAp.point(k(x+y))
          case Mult(x@ElGamalEnc(_,_),y@ElGamalEnc(_,_),k) => FreeAp.point(k(x*y))
          case x => FreeAp.lift(x)
        }
      }
    })
  }

  // perform all explicit conversions
  def preconvert[A](keyRing: KeyRing)(p: Crypto[A]): Crypto[A] = {
    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[B](fa: CryptoF[B]): Crypto[B] = {
        fa match {
          case ToPaillier(v,k) =>
            val r@PaillierEnc(_) = Common.convert(keyRing)(Additive, v)
            FreeAp.point(k(r))
          case ToGamal(v,k) =>
            val r@ElGamalEnc(_,_) = Common.convert(keyRing)(Multiplicative, v)
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

  def extractNumbers[A](p: Crypto[A]): List[(Option[Scheme],Enc)] = {
    p.analyze(new (CryptoF ~> λ[α => List[(Option[Scheme],Enc)]]) {
      def apply[B](a: CryptoF[B]) = a match {
        case ToPaillier(v,k) => List((Some(Additive),v))
        case ToGamal(v,k) => List((Some(Multiplicative),v))
        case ToAes(v,k) => List((Some(Equality),v))
        case ToOpe(v,k) => List((Some(Comparable),v))

        case Mult(lhs,rhs,k) => List((Some(Multiplicative),lhs),(Some(Multiplicative),rhs))
        case Plus(lhs,rhs,k) => List((Some(Additive),lhs),(Some(Additive),rhs))
        case Equals(lhs,rhs,k) => List((Some(Equality),lhs),(Some(Equality),rhs))
        case Compare(lhs,rhs,k) => List((Some(Comparable),lhs),(Some(Comparable),rhs))

        case Sub(lhs,rhs,k) => List((None,lhs),(None,rhs))
        case Div(lhs,rhs,k) => List((None,lhs),(None,rhs))

        case Encrypt(s,v,k) => List()
        case Embed(p,k) => extractNumbers(p)
      }
    }).reverse
  }

  type StateCrypto[α] = State[List[Enc],Crypto[α]]
  def replaceNumbers[A](p: Crypto[A]): StateCrypto[A] = {
    implicit val ev = Applicative[λ[α => State[List[Enc],α]]].compose[Crypto]

    def takeHead(): State[List[Enc],Enc] = for {
      head <- State.gets{ (s:List[Enc]) =>
        s.headOption.getOrElse(sys.error("Not enough numbers for replacement"))
      }
      _ <- State.modify((s:List[Enc]) => s.tail)
    } yield head

    p.foldMap[StateCrypto](new (CryptoF ~> StateCrypto) {
      def apply[B](fa: CryptoF[B]): StateCrypto[B] = fa match {
        case ToPaillier(v,k) => for {
          head <- takeHead()
        } yield FreeAp.lift(ToPaillier(head,k))
        case ToGamal(v,k) => for {
          head <- takeHead()
        } yield FreeAp.lift(ToGamal(head,k))
        case ToAes(v,k) => for {
          head <- takeHead()
        } yield FreeAp.lift(ToAes(head,k))
        case ToOpe(v,k) => for {
          head <- takeHead()
        } yield FreeAp.lift(ToOpe(head,k))
        case Mult(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Mult(l,r,k))
        case Plus(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Plus(l,r,k))
        case Equals(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Equals(l,r,k))
        case Compare(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Compare(l,r,k))
        case Sub(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Sub(l,r,k))
        case Div(lhs,rhs,k) => for {
          l <- takeHead()
          r <- takeHead()
        } yield FreeAp.lift(Div(l,r,k))
        case Encrypt(s,v,k) => State.state(FreeAp.lift(Encrypt(s,v,k)))
        case Embed(p,k) => State.state(FreeAp.lift(Embed(p,k)))
      }
    })(ev)
  }
}

object AnalysisMain extends App {
  val keyRing = KeyRing.create
  val interpreter = LocalInterpreter(keyRing)

  val xs = SampleData.fixed1.map(Common.encrypt(Multiplicative, keyRing)).take(300)

  val prog = sumA(Common.zero(keyRing))(xs)

  println(Common.decrypt(keyRing.priv)(interpreter.interpretA(prog)))

  println(s"Required conversions: ${Analysis.requiredConversions(prog)}")
  val nums = Analysis.extractNumbers(prog)
  println(s"Num extracted: ${nums.length}")
  println(s"Extracted numbers == original?: ${nums == xs}")

  val newXs = nums.map { case (_,num) =>
    (Common.convert(keyRing)(Additive, num))
  }

  println("Replacing...")
  val newProg = Analysis.replaceNumbers(prog).eval(newXs)
  println("Done replacing")
  val nums2 = Analysis.extractNumbers(newProg)
  println(s"Num extracted: ${nums2.length}")
  println(s"Required conversions after optimization: ${Analysis.requiredConversions(newProg)}")

  val r = interpreter.interpretA(newProg)
  println(Common.decrypt(keyRing.priv)(r))
}
