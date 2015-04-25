package crypto.cipher

import org.scalacheck.Arbitrary.arbitrary

object TestUtils {
  val positiveInts = arbitrary[BigInt] suchThat (_.signum == 1)
}
