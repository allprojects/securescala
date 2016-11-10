package crypto.cipher

import argonaut._
import Argonaut._
import scala.math.Ordering.Implicits._

case class CipherDomain[A:Ordering](min: A, max: A) {
  def contains(e: A) = e >= min && e <= max
  /** Create a new CipherDomain that */
}

object CipherDomain {
  implicit def decode[A:Ordering:DecodeJson]: DecodeJson[CipherDomain[A]] = {
    DecodeJson(c => for {
      min <- (c --\ "min").as[A]
      max <- (c --\ "max").as[A]
    } yield CipherDomain(min,max))
  }

  implicit def encode[A:Ordering:EncodeJson]: EncodeJson[CipherDomain[A]] = {
    EncodeJson((c: CipherDomain[A]) =>
      ("min" := c.min) ->: ("max" := c.max) ->: jEmptyObject
    )
  }
}
