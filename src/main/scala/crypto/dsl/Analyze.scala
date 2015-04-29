package crypto.dsl

import scalaz._
import scalaz.std.anyVal._

import CryptoF.Dsl._
import crypto.cipher._

object Analysis {
  def requiredConversions[A](program: Crypto[A]): Int = {
    program.analyze(new (CryptoF ~> λ[α => Int]) {
      def apply[B](fa: CryptoF[B]): Int = fa match {
        case ToPaillier(PaillierEnc(_),_) => 0
        case ToGamal(GamalEnc(_,_),_) => 0
        case ToAes(AesEnc(_),_) => 0

        case Plus(PaillierEnc(_),PaillierEnc(_),_) => 0
        case Plus(_,PaillierEnc(_),_) => 1
        case Plus(PaillierEnc(_),_,_) => 1
        case Plus(_,_,_) => 2

        case Mult(GamalEnc(_,_),GamalEnc(_,_),_) => 0
        case Mult(_,GamalEnc(_,_),_) => 1
        case Mult(GamalEnc(_,_),_,_) => 1
        case Mult(_,_,_) => 2

        // TODO cases for the other
        case x => 0
      }
    })
  }

  def optimize[A](p: Crypto[A]): Crypto[A] = {
    p.foldMap(new (CryptoF ~> Crypto) {
      def apply[A](fa: CryptoF[A]): Crypto[A] = {
        fa match {
          case ToPaillier(p@PaillierEnc(_),k) => FreeAp.point(k(p))
          case ToGamal(g@GamalEnc(_,_),k) => FreeAp.point(k(g))
          case Plus(x@PaillierEnc(_),y@PaillierEnc(_),k) => FreeAp.point(k(x+y))
          case x => FreeAp.lift(x)
        }
      }
    })
  }
}
