package example

import scala.annotation.tailrec

object MathematicalOperations:

  def square(x: Double) = x * x
  def abs(x: Double) = if x > 0 then x else -x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if isdGoodEnough(guess) then guess
      else sqrtIter(improve(guess))

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def isdGoodEnough(guess: Double) =
      abs(square(guess) - x) < 0.001

    sqrtIter(1.0)
  }

  @main def test = println(sqrt(2))
  @main def test2 = println(sqrt(49))

  /**
   * Greatest common divisor between two numbers
   * @param a the first number to evaluate
   * @param b the second number to evaluate
   * @return the greatest common divisor between given numbers
   * */
  @tailrec
  def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

  @main def testGcd = print(gcd(60, 100))


  /**
   * Factorial
   * @param n the number to evaluate
   * @return the factorial of the given number
   * */
  def factorial(n: Int): Int = if n == 0 then 1 else n * factorial(n - 1)

  def factorialTailRec(n: BigInt): BigInt =
    @tailrec
    def loop(acc: BigInt, n: BigInt): BigInt =
      if n <= 1 then acc
      else loop(acc * n, n -1)
    loop(1, n)


  @main def testFactorial = println(factorial(4))
  @main def testFactorialRec = println(factorial(10))
