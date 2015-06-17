package crypto.dsl

import crypto._
import crypto.cipher._
import scalaz._
import scalaz.syntax.bind._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._

case class LocalInterpreter(keyRing: KeyRing) extends PureCryptoInterpreter {
  private def doConvert(s: Scheme, in: EncInt) = Common.depConvert(keyRing)(s,in)
  private def additive(x: EncInt): PaillierEnc = doConvert(Additive, x)
  private def multiplicative(x: EncInt): ElGamalEnc = doConvert(Multiplicative, x)
  private def equality(x: EncInt): AesEnc = doConvert(Equality, x)
  private def comparable(x: EncInt): OpeEnc = doConvert(Comparable, x)
  private def equalityStr(x: EncString): AesString =
    Common.encryptStrAes(keyRing)(Common.decryptStr(keyRing)(x))
  private def comparableStr(x: EncString): OpeString =
    Common.encryptStrOpe(keyRing)(Common.decryptStr(keyRing)(x))

  override def interpret[A](p: CryptoM[A]): A = p.resume match {
    case -\/(next) => next match {
      case Coproduct(-\/(cf)) => cf match {
        case Mult(lhs@ElGamalEnc(_,_),rhs@ElGamalEnc(_,_),k) =>
          interpret(k(lhs * rhs))
        case Mult(lhs,rhs,k) =>
          interpret(k(multiplicative(lhs)*multiplicative(rhs)))

        case Plus(lhs@PaillierEnc(_),rhs@PaillierEnc(_),k) =>
          interpret(k(lhs+rhs))
        case Plus(lhs,rhs,k) =>
          interpret(k(additive(lhs) + additive(rhs)))

        case Compare(lhs@OpeEnc(_),rhs@OpeEnc(_),k) =>
          interpret(k(lhs ?|? rhs))
        case Compare(lhs,rhs,k) =>
          interpret(k(comparable(lhs) ?|? comparable(rhs)))

        case CompareStr(lhs@OpeString(_),rhs@OpeString(_),k) =>
          interpret(k(lhs ?|? rhs))
        case CompareStr(lhs,rhs,k) =>
          interpret(k(comparableStr(lhs) ?|? comparableStr(rhs)))

        case ConcatStr(lhs@OpeString(_),rhs@OpeString(_),k) =>
          interpret(k(lhs |+| rhs))
        case ConcatStr(lhs,rhs,k) =>
          interpret(k(comparableStr(lhs) |+| comparableStr(rhs)))

        case Equals(lhs@AesEnc(_),rhs@AesEnc(_),k) =>
          interpret(k(lhs === rhs))
        case Equals(lhs,rhs,k) =>
          interpret(k(equality(lhs) === equality(rhs)))

        case EqualsStr(lhs@AesString(_),rhs@AesString(_),k) =>
          interpret(k(lhs === rhs))
        case EqualsStr(lhs,rhs,k) =>
          interpret(k(equalityStr(lhs) === equalityStr(rhs)))

        case Encrypt(s,v,k) => interpret(k(Common.encrypt(s, keyRing)(v)))

        case ToPaillier(v,k) => interpret(k(additive(v)))
        case ToGamal(v,k) => interpret(k(multiplicative(v)))
        case ToAes(v,k) => interpret(k(equality(v)))
        case ToOpe(v,k) => interpret(k(comparable(v)))
        case ToAesStr(v,k) => interpret(k(equalityStr(v)))
        case ToOpeStr(v,k) => interpret(k(comparableStr(v)))

          // Offline operations

        case Sub(lhs,rhs,k) =>
          val plainLhs = Common.decrypt(keyRing.priv)(lhs)
          val plainRhs = Common.decrypt(keyRing.priv)(rhs)
          val r = Common.encrypt(Additive, keyRing)(plainLhs - plainRhs)
          interpret(k(r))

        case Div(lhs,rhs,k) =>
          val plainLhs = Common.decrypt(keyRing.priv)(lhs)
          val plainRhs = Common.decrypt(keyRing.priv)(rhs)
          val r = Common.encrypt(Additive, keyRing)(plainLhs / plainRhs)
          interpret(k(r))

        case IsEven(v,k) =>
          val plain = Common.decrypt(keyRing.priv)(v)
          interpret(k(plain.mod(2) == 0))

        case IsOdd(v,k) =>
          val plain = Common.decrypt(keyRing.priv)(v)
          interpret(k(plain.mod(2) == 1))
      }
      case Coproduct(\/-(e@Embed())) =>
        val r: CryptoM[A] = e.k(empower(Free.point(interpretA(e.v)))).join
        interpret(r)
    }
    case \/-(x) => x
  }
}
