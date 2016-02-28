object PrimeChecker {
  def isPrime(number: Int): Boolean = {
    (2 until number) forall (x => number % x != 0)
  }
}

