package crypto.dsl

import scalaz._
import scalaz.std.list

import CryptoF.DSL._
import crypto.cipher._
import crypto.KeyRing

trait CryptoInterpreter {
  def interpret[A]: CryptoM[A] => A
}

case class LocalInterpreter(keyRing: KeyRing) extends CryptoInterpreter {
  val KeyRing(encKeys,decKeys) = keyRing
  def interpret[A]: CryptoM[A] => A = _.resume match {
    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) =>
      val lhs2@GamalEnc(_,_) = Common.convert(encKeys, decKeys)(Multiplicative, lhs)
      val rhs2@GamalEnc(_,_) = Common.convert(encKeys, decKeys)(Multiplicative, rhs)
      interpret(k(lhs2*rhs2))
    case -\/(Plus(PaillierEnc(lhs),PaillierEnc(rhs),k)) =>
      val r = PaillierEnc((lhs * rhs) mod encKeys.paillier.nSquare)
      interpret(k(r))
    case -\/(Plus(lhs,rhs,k)) =>
      val PaillierEnc(lhs_) = Common.convert(encKeys, decKeys)(Additive, lhs)
      val PaillierEnc(rhs_) = Common.convert(encKeys, decKeys)(Additive, rhs)
      val r = PaillierEnc((lhs_ * rhs_) mod encKeys.paillier.nSquare)
      interpret(k(r))
    case -\/(Compare(lhs,rhs,k)) => sys.error("No scheme for comparison")
    case -\/(Equals(lhs,rhs,k)) => sys.error("No scheme for equality")
    case -\/(Encrypt(v,k)) =>
      // TODO be more clever about what scheme to use
      interpret(k(NoEnc(v)))
    case \/-(x) => x
  }
}
