import scala.annotation.tailrec

object PalindromeChecker {
  def checkReverse(s: String): Boolean = s == s.reverse

  def checkIndices(s: String): Boolean = {
    @tailrec
    def loop(i: Int, j: Int): Boolean = (i, j) match {
      case (i, j) if i + 1 == j => s(i) == s(j)
      case (i, j) if i == j => true
      case (i, j) if s(i) != s(j) => false
      case (i, j) if s(i) == s(j) => loop(i + 1, j - 1)
    }

    if (s.isEmpty) true
    else loop(0, s.length - 1)
  }
}
