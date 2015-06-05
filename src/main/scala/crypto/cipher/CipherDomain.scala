package crypto.cipher

import scala.math.Ordering.Implicits._

case class CipherDomain[A:Ordering](min: A, max: A) {
  def contains(e: A) = e >= min && e <= max
  /** Create a new CipherDomain that */
}
