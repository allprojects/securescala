package crypto.dsl

import scala.language.higherKinds

/**
  * An interpreter for a program written using the crypto dsl.  It
  * offers different implementations of programs depending on the
  * style it is written in (applicative vs monadic)
  */

trait CryptoInterpreter[F[_]] {
  def apply[A](p: CryptoM[A]): F[A] = interpret(p)

  /**
    * Interpret a program written in the monadic DSL and return the result
    */
  def interpret[A](p: CryptoM[A]): F[A]

  /**
    * Interpret a program written in the applicative DSL and therefore
    * does not depend on the order of the effects.
    *
    * By default this performs no optimizations, you need to override
    * it and take advantage of the applicative structure
    */
  def interpretA[A](p: Crypto[A]): F[A] = interpret(p.monadic)
}

/**
  * A CryptoInterpreter that does not make use of the higher kinded
  * type parameter
  *
  * Why λ[α=>α]?
  *   - This is equivalent to `type Identity[A] = A` enabling the
  *     interpreter to return a type that is not higher kinded
  * (If you ask yourself what is going on here: https://github.com/non/kind-projector)
  */
trait PureCryptoInterpreter extends CryptoInterpreter[λ[α=>α]]
