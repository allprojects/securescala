package crypto.dsl

import scalaz._
import scalaz.std.list
import scalaz.syntax.bind._

import crypto.cipher._
import crypto._

trait CryptoInterpreter {
  def interpret[A]: CryptoM[A] => A
  def interpretA[A]: Crypto[A] => A
}

case class LocalInterpreter(keyRing: KeyRing) extends CryptoInterpreter {
  val KeyRing(encKeys,decKeys) = keyRing

  // TODO take advantage of applicative structure to do optimizations
  def interpretA[A]: Crypto[A] => A = x => {
    println("Game changing optimization taking place...")
    interpret(x.monadic)
  }

  def interpret[A]: CryptoM[A] => A = _.resume match {
    // Multiplication
    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) =>
      val lhs2@GamalEnc(_,_) = Common.convert(encKeys, decKeys)(Multiplicative, lhs)
      val rhs2@GamalEnc(_,_) = Common.convert(encKeys, decKeys)(Multiplicative, rhs)
      interpret(k(lhs2*rhs2))

    // Addition
    case -\/(Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k)) => interpret(k(lhs+rhs))
    case -\/(Plus(lhs,rhs,k)) =>
      val PaillierEnc(lhs_) = Common.convert(encKeys, decKeys)(Additive, lhs)
      val PaillierEnc(rhs_) = Common.convert(encKeys, decKeys)(Additive, rhs)
      val r = PaillierEnc((lhs_ * rhs_) mod encKeys.paillier.nSquare)
      interpret(k(r))

    // Comparisons
    case -\/(Compare(lhs,rhs,k)) => sys.error("No scheme for comparison")

    // Equality
    case -\/(Equals(lhs@AesEnc(_),rhs@AesEnc(_),k)) => interpret(k(lhs =:= rhs))
    case -\/(Equals(lhs,rhs,k)) =>
      val decLhs = Common.decrypt(decKeys)(lhs).toByteArray
      val decRhs = Common.decrypt(decKeys)(rhs).toByteArray

      val encLhs: AesEnc = AesEnc(decKeys.aesEnc(decLhs))
      val encRhs: AesEnc = AesEnc(decKeys.aesEnc(decRhs))
      interpret(k(encLhs =:= encRhs))

    // Encryption
    case -\/(Encrypt(v,k)) =>
      // TODO be more clever about what scheme to use
      interpret(k(NoEnc(v)))

    case -\/(ToPaillier(v,k)) =>
      val r@PaillierEnc(_) = Common.convert(encKeys,decKeys)(Additive,v)
      interpret(k(r))

    case -\/(ToGamal(v,k)) =>
      val r@GamalEnc(_,_) = Common.convert(encKeys,decKeys)(Multiplicative,v)
      interpret(k(r))

    case -\/(ToAes(v,k)) =>
      val r = Common.decrypt(decKeys)(v)
      interpret(k(AesEnc(decKeys.aesEnc(r.toByteArray))))

      // Offline operations?
    case -\/(Sub(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(decKeys)(lhs)
      val plainRhs = Common.decrypt(decKeys)(rhs)
      val r = Common.encrypt(Additive, encKeys)(plainLhs - plainRhs)
      interpret(k(r))

    case -\/(Div(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(decKeys)(lhs)
      val plainRhs = Common.decrypt(decKeys)(rhs)
      val r = Common.encrypt(Additive, encKeys)(plainLhs / plainRhs)
      interpret(k(r))

    case -\/(Embed(p,k)) =>
      val r: CryptoM[A] = k(Free.point(interpretA(p))).join
      interpret(r)

    // End of the program, return the final value
    case \/-(x) => x
  }
}
