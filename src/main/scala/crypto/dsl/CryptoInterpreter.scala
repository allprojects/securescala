package crypto.dsl

import scala.language.higherKinds

/**
  * An interpreter for a program written using the crypto dsl.  It
  * offers different implementations of programs depending on the
  * style it is written in (applicative vs monadic)
  */

trait CryptoInterpreter[F[_]] {
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
