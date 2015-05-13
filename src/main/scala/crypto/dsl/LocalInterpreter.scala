package crypto.dsl

import scalaz._
import scalaz.std.scalaFuture._
import scalaz.syntax.bind._
import scalaz.syntax.order._

import scala.concurrent._

import crypto._
import crypto.cipher._

// Why λ[α=>α]?
// - This is equivalent to `type Identity[A] = A` enabling the
//   interpreter to return a type that is not higher kinded
case class LocalInterpreter(keyRing: KeyRing) extends CryptoInterpreter[λ[α=>α]] {
  // One possible optimization is to use futures
  def interpPar[A](p: Crypto[A])(implicit C: ExecutionContext): Future[A] = {

    p.foldMap(new (CryptoF ~> Future) {
      // Peform regular interpretation inside future
      def apply[A](fa: CryptoF[A]): Future[A] = Future { interpret(Free.liftF(fa)) }
    })
  }

  private def doConvert(s: Scheme, in: Enc) = Common.depConvert(keyRing)(s,in)
  private def additive(x: Enc): PaillierEnc = doConvert(Additive, x)
  private def multiplicative(x: Enc): GamalEnc = doConvert(Multiplicative, x)
  private def equality(x: Enc): AesEnc = doConvert(Equality, x)
  private def comparable(x: Enc): OpeEnc = doConvert(Comparable, x)

  def interpret[A]: CryptoM[A] => A = _.resume match {

    case -\/(Mult(lhs@GamalEnc(_,_),rhs@GamalEnc(_,_),k)) => interpret(k(lhs * rhs))
    case -\/(Mult(lhs,rhs,k)) => interpret(k(multiplicative(lhs)*multiplicative(rhs)))

    case -\/(Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k)) => interpret(k(lhs+rhs))
    case -\/(Plus(lhs,rhs,k)) => interpret(k(additive(lhs) + additive(rhs)))

    case -\/(Compare(lhs@OpeEnc(_),rhs@OpeEnc(_),k)) => interpret(k(lhs ?|? rhs))
    case -\/(Compare(lhs,rhs,k)) => interpret(k(comparable(lhs) ?|? comparable(rhs)))

    case -\/(Equals(lhs@AesEnc(_),rhs@AesEnc(_),k)) => interpret(k(lhs =:= rhs))
    case -\/(Equals(lhs,rhs,k)) => interpret(k(equality(lhs) =:= equality(rhs)))

    case -\/(Encrypt(s,v,k)) => interpret(k(Common.encrypt(s, keyRing)(v)))

    case -\/(ToPaillier(v,k)) => interpret(k(additive(v)))

    case -\/(ToGamal(v,k)) => interpret(k(multiplicative(v)))

    case -\/(ToAes(v,k)) => interpret(k(equality(v)))

    case -\/(ToOpe(v,k)) => interpret(k(comparable(v)))

    // Offline operations

    case -\/(Sub(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(keyRing.priv)(lhs)
      val plainRhs = Common.decrypt(keyRing.priv)(rhs)
      val r = Common.encrypt(Additive, keyRing)(plainLhs - plainRhs)
      interpret(k(r))

    case -\/(Div(lhs,rhs,k)) =>
      val plainLhs = Common.decrypt(keyRing.priv)(lhs)
      val plainRhs = Common.decrypt(keyRing.priv)(rhs)
      val r = Common.encrypt(Additive, keyRing)(plainLhs / plainRhs)
      interpret(k(r))

    case -\/(Embed(p,k)) =>
      val r: CryptoM[A] = k(Free.point(interpretA(p))).join
      interpret(r)

    case \/-(x) => x
  }
}
