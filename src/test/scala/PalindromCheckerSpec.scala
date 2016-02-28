import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

object PalindromCheckerSpec extends Properties("palindromes") {

  val palindromeGen: Gen[String] = for {
    base <- Gen.alphaStr
    middle <- Gen.option(arbitrary[Char])
  } yield base + middle.getOrElse("") + base.reverse

  val maybePalindrome = Gen.oneOf(palindromeGen, arbitrary[String])

  property("checkReverse coincides with generator") =
    forAll(palindromeGen) { palindrome =>
      PalindromeChecker.checkReverse(palindrome)
    }

  property("naive check Indices") =
    forAll(maybePalindrome) { maybe =>
      PalindromeChecker.checkReverse(maybe) == PalindromeChecker.checkIndices(maybe)
    }
}
