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


  @main def testFactorial = println(factorial(3))
  @main def testFactorialRec = println(factorial(10))

  /**
   * Sum the integers between two numbers
   * @param a the first number
   * @param b the second number
   * @return the numbers between them
   * */
  def numsBetween(a: Int, b: Int): Int =
    if a > b then 0 else a + numsBetween(a + 1, b)

  @main def testNumsBetween = println(numsBetween(1,4))

  /**
   * Cube of number
   * @param x the number to multiply
   * @return the cube for the given number
   * */
  def cube(x: Int): Int = x * x * x

  @main def testCube = println(cube(3))

  /**
   * Sum cubes between to numbers
   * @param a the first number
   * @param b the second number
   * @return the sum of the cubes between them
   * */
  def sumCubes(a: Int, b: Int): Int =
    if a > b then 0 else cube(a) + sumCubes(a + 1, b)

  @main def testSumCubes = print(sumCubes(1,4))


  /**
   * Sum factorials between two numbers
   * @param a the first number
   * @param b the second number
   * @return the sum of the factorials of numbers between them
   * */
  def sumFactorials(a: Int, b: Int): Int =
    if a > b then 0 else factorial(a) + sumFactorials(a + 1, b)

  @main def testSumFactorials = println(sumFactorials(1, 2))

  /**
   * responde si un número es primo, es decir, es divisible solo por 1 y por el mismo
   * @param n es el número a ananlizar
   * @return si es o no primo
   * */
  def isPrime(n: Int): Boolean = (2 until n).forall(n % _ != 0)

  @main def testIsPrime = println(isPrime(2))

class Polynom(nonZeroTerms: Map[Int, Double]):
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = nonZeroTerms.withDefaultValue(0.0)

//  def + (other: Polynom): Polynom =
//    Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

  def + (other: Polynom): Polynom =
    Polynom(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))

  override def toString: String =
    val termStrings =
      for (exp, coeff) <- terms.toList.sorted.reverse
        yield
          val exponent = if exp == 0 then "" else s"x^$exp"
          s"$coeff$exponent"
    if terms.isEmpty then "0"
    else termStrings.mkString("+")

val polynom1 = Polynom(0 -> 2, 1 -> -3, 2 -> 1)
val polynom2 = Polynom(Map())

@main def testSumPolynom = println(polynom1 + polynom1 + polynom2)


