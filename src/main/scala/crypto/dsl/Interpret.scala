package crypto.dsl

import scalaz._
import scalaz.std.list

import CryptoF.DSL._
import crypto.cipher._

trait CryptoInterpreter {
  def interpret[A]: CryptoM[A] => A
}

case class LocalInterpreter(encKeys: EncKeys, decKeys: DecKeys) extends CryptoInterpreter {
  def interpret[A]: CryptoM[A] => A = _.resume match {
    case -\/(Mult(lhs,rhs,k)) => sys.error("No scheme for multiplication")
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
