package crypto.dsl

import scalaz._
import scalaz.Dual._
import scalaz.Free._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.syntax.monad._

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

        case Equals(AesEnc(_),AesEnc(_),_) => 0
        case Equals(_,AesEnc(_),_) => 1
        case Equals(AesEnc(_),_,_) => 1
        case Equals(_,_,_) => 2

        case Compare(OpeEnc(_),OpeEnc(_),_) => 0
        case Compare(OpeEnc(_),_,_) => 1
        case Compare(_,OpeEnc(_),_) => 1
        case Compare(_,_,_) => 2

        case Sub(_,_,_) => 2
        case Div(_,_,_) => 2

        case IsEven(_,_) => 0
        case IsOdd(_,_) => 0
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

  // perform all explicit conversions locally
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

  def withNumbers[A](p: Crypto[A])(
    f: List[(Option[Scheme],EncInt)] => List[EncInt]): Crypto[A] = {

    val nums = extractNumbers(p)
    val newNums = f(nums)
    if (newNums.length != nums.length) {
      sys.error(s"withNumbers: The given function returned an invalid list:" +
        s"expected size ${nums.length} but was ${newNums.length}")
    }

    replaceNumbers(p).eval(newNums).run
  }

  def mapNumbers[A](p: Crypto[A])(f: (Option[Scheme],EncInt) => EncInt): Crypto[A] =
    withNumbers(p)(_.map { case (optScheme,enc) => f(optScheme,enc) })

  def extractConversions(p: Crypto[_]): List[(Scheme,EncInt)] = {
    Tag.unwrap(
      p.analyze(new (CryptoF ~> λ[α => DList[(Scheme,EncInt)] @@ Tags.Dual]) {
        def apply[B](a: CryptoF[B]) = a match {
          case ToPaillier(v,k) => Dual(DList((Additive,v)))
          case ToGamal(v,k) => Dual(DList((Multiplicative,v)))
          case ToAes(v,k) => Dual(DList((Equality,v)))
          case ToOpe(v,k) => Dual(DList((Comparable,v)))

          case Mult(lhs,rhs,k) => Dual(DList((Multiplicative,lhs),(Multiplicative,rhs)))
          case Plus(lhs,rhs,k) => Dual(DList(((Additive),lhs),(Additive,rhs)))
          case Equals(lhs,rhs,k) => Dual(DList((Equality,lhs),(Equality,rhs)))
          case EqualsStr(_,_,k) => Dual(DList())
          case Compare(lhs,rhs,k) => Dual(DList((Comparable,lhs),(Comparable,rhs)))

          case Sub(lhs,rhs,k) => Dual(DList())
          case Div(lhs,rhs,k) => Dual(DList())
          case IsEven(v,k) => Dual(DList())
          case IsOdd(v,k) => Dual(DList())
          case Encrypt(s,v,k) => Dual(DList())
          case Embed(p,k) => sys.error("impossible")
        }
      })).toList
  }

  // TODO hoist + retract better?
  type StateCrypto[α] = StateT[Trampoline,List[EncInt],Crypto[α]]
  def cryptoState = StateT.stateTMonadState[List[EncInt],Trampoline]

  def replaceConversions[A](p: Crypto[A]): StateCrypto[A] = {
    implicit val ev = cryptoState.compose[Crypto]

    p.foldMap[StateCrypto](new (CryptoF ~> StateCrypto) {
      def apply[B](fa: CryptoF[B]): StateCrypto[B] = fa match {
        case ToPaillier(v,k) => takeHead().map(head => FreeAp.lift(ToPaillier(head,k)))
        case ToGamal(v,k) => takeHead().map(head => FreeAp.lift(ToGamal(head,k)))
        case ToAes(v,k) => takeHead().map(head => FreeAp.lift(ToAes(head,k)))
        case ToOpe(v,k) => takeHead().map(head => FreeAp.lift(ToOpe(head,k)))
        case Mult(lhs,rhs,k) => take2Head().map { case(l,r) => FreeAp.lift(Mult(l,r,k)) }
        case Plus(lhs,rhs,k) => take2Head().map { case(l,r) => FreeAp.lift(Plus(l,r,k)) }
        case Equals(lhs,rhs,k) =>
          take2Head().map { case(l,r) => FreeAp.lift(Equals(l,r,k)) }
        case Compare(lhs,rhs,k) =>
          take2Head().map { case(l,r) => FreeAp.lift(Compare(l,r,k)) }
        case Sub(lhs,rhs,k) => cryptoState.state(FreeAp.lift(fa))
        case Div(lhs,rhs,k) => cryptoState.state(FreeAp.lift(fa))
        case IsEven(v,k) => cryptoState.state(FreeAp.lift(fa))
        case IsOdd(v,k) => cryptoState.state(FreeAp.lift(fa))
        case Encrypt(s,v,k) => cryptoState.state(FreeAp.lift(fa))
        case EqualsStr(_,_,_) => cryptoState.state(FreeAp.lift(fa))
        case Embed(p,k) => sys.error("impossible")
      }
    })(ev)
  }

  def extractNumbers[A](p: Crypto[A]): List[(Option[Scheme],EncInt)] = {
    Tag.unwrap(
      p.analyze(new (CryptoF ~> λ[α => DList[(Option[Scheme],EncInt)] @@ Tags.Dual]) {
        def apply[B](a: CryptoF[B]) = a match {
          case ToPaillier(v,k) => Dual(DList((Some(Additive),v)))
          case ToGamal(v,k) => Dual(DList((Some(Multiplicative),v)))
          case ToAes(v,k) => Dual(DList((Some(Equality),v)))
          case ToOpe(v,k) => Dual(DList((Some(Comparable),v)))

          case Mult(lhs,rhs,k) =>
            Dual(DList((Some(Multiplicative),lhs),(Some(Multiplicative),rhs)))
          case Plus(lhs,rhs,k) => Dual(DList((Some(Additive),lhs),(Some(Additive),rhs)))
          case Equals(lhs,rhs,k) => Dual(DList((Some(Equality),lhs),(Some(Equality),rhs)))
          case Compare(lhs,rhs,k) => Dual(DList((Some(Comparable),lhs),(Some(Comparable),rhs)))

          case Sub(lhs,rhs,k) => Dual(DList((None,lhs),(None,rhs)))
          case Div(lhs,rhs,k) => Dual(DList((None,lhs),(None,rhs)))
          case IsEven(v,k) => Dual(DList((None,v)))
          case IsOdd(v,k) => Dual(DList((None,v)))

          case Encrypt(s,v,k) => Dual(DList())
          case Embed(p,k) => sys.error("impossible")
          case EqualsStr(_,_,k) => Dual(DList())
        }
      })).toList
  }

  // TODO hoist + retract better?
  def replaceNumbers[A](p: Crypto[A]): StateCrypto[A] = {
    implicit val ev = cryptoState.compose[Crypto]

    p.foldMap(new (CryptoF ~> StateCrypto) {
      def apply[B](fa: CryptoF[B]) = fa match {
        case ToPaillier(v,k) => takeHead().map(h => FreeAp.lift(ToPaillier(h,k)))
        case ToGamal(v,k) => takeHead().map(h => FreeAp.lift(ToGamal(h,k)))
        case ToAes(v,k) => takeHead().map(h => FreeAp.lift(ToAes(h,k)))
        case ToOpe(v,k) => takeHead().map(h => FreeAp.lift(ToOpe(h,k)))
        case Mult(lhs,rhs,k) => take2Head().map { case (l,r) => FreeAp.lift(Mult(l,r,k)) }
        case Plus(lhs,rhs,k) => take2Head().map { case (l,r) => FreeAp.lift(Plus(l,r,k)) }
        case Equals(lhs,rhs,k) =>
          take2Head().map { case (l,r) => FreeAp.lift(Equals(l,r,k)) }
        case Compare(lhs,rhs,k) =>
          take2Head().map { case (l,r) => FreeAp.lift(Compare(l,r,k)) }
        case Sub(lhs,rhs,k) => take2Head().map { case (l,r) => FreeAp.lift(Sub(l,r,k)) }
        case Div(lhs,rhs,k) => take2Head().map { case (l,r) => FreeAp.lift(Div(l,r,k)) }
        case IsEven(v,k) => takeHead().map(h => FreeAp.lift(IsEven(h,k)))
        case IsOdd(v,k) => takeHead().map(h => FreeAp.lift(IsOdd(h,k)))
        case Encrypt(_,_,_) => cryptoState.state(FreeAp.lift(fa))
        case EqualsStr(_,_,_) => cryptoState.state(FreeAp.lift(fa))
        case Embed(p,k) => sys.error("impossible")
      }
    })(ev)
  }

  private def takeHead(): StateT[Trampoline,List[EncInt],EncInt] = for {
    head <- cryptoState.gets{ (s:List[EncInt]) =>
      s.headOption.getOrElse(sys.error("Not enough numbers for replacement"))
    }
    _ <- cryptoState.modify((s:List[EncInt]) => s.tail)
  } yield head

  private def take2Head(): StateT[Trampoline,List[EncInt],(EncInt,EncInt)] =
    takeHead() tuple takeHead()

  import scalaz.syntax.traverse._
  import scalaz.Free._

  def zipIndex[A](as: List[A]): List[(Int,A)] =
    as.traverseU(x => for {
      n <- State.get[Int]
      _ <- State.modify((n:Int)=>n+1)
    } yield (n,x)).eval(0)


  def zipIndex2[A](as: List[A]): List[(Int,A)] =
    as.traverseU(x => for {
      n <- StateT.stateTMonadState[Int,Trampoline].get
      _ <- StateT.stateTMonadState[Int,Trampoline].modify((n:Int)=>n+1)
    } yield (n,x)).eval(0).run
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
  val newProg = Analysis.replaceNumbers(prog).eval(newXs).run
  println("Done replacing")
  val nums2 = Analysis.extractNumbers(newProg)
  println(s"Num extracted: ${nums2.length}")
  println(s"Required conversions after optimization: ${Analysis.requiredConversions(newProg)}")

  val r = interpreter.interpretA(newProg)
  println(Common.decrypt(keyRing.priv)(r))
}
