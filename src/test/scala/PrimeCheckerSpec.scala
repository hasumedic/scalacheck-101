import org.scalacheck._
import org.scalacheck.Prop.forAll

object PrimeCheckerSpec extends Properties("prime") {

  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail filter (_ % s.head != 0))
  }

  val primeGen: Gen[Int] = {
    val randomVal = scala.util.Random.nextInt(1000)
    sieve(from(2)).drop(randomVal).head
  }

  property("checkReverse coincides with generator") =
    forAll(primeGen) { prime =>
      PrimeChecker.isPrime(prime)
    }
}
