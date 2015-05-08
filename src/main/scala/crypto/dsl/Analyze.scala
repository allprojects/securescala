package crypto.dsl

import scalaz._
import scalaz.std.anyVal._

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
  def optimize[A](p: Crypto[A]): Crypto[A] = {
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
}
