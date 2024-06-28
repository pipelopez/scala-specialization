package example

import example.MathematicalOperations.{cube, factorial}

object Currying:
  /**
  * Sum all numbers between two given numbers, but applying a function to everyone before
  * @param f the function to apply to the founded number between two given numbers
  * @return the sum of numbers between a and b, but applying a function to everyone previously
  * */
  def sum(f: Int => Int): (Int, Int) => Int =
    def sumF(a: Int, b: Int): Int =
      if a > b then 0
      else f(a) + sumF(a + 1, b)
    sumF

  @main def testSumCurry = println(sum(cube) (1,3))

  /**
   * Same as the below, but with other kind of notation, it returns a function that receives
   * 2 parameters of type Int and return an Int
   * @param f the function to apply
   * @param a the first number
   * @param b the second number
   * */
  def sum2(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 0 else f(a) + sum2(f)(a + 1, b)

  @main def testSum2 = println(sum(cube) (1,3))

  /**
   * This one receives a function and returns another function that returns an Int that
   * is the result of a multiply the values between a and b parameters
   * @param f that is a function to apply
   * @param a that is a first parameter in the return function
   * @param b that is the second parameter in the return function
   * */
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1 else f(a) * product(f)(a + 1 , b)

  @main def testProduct = println(product(x => x * x)(1, 5))

  /**
   * Factorial function definition using like auxiliary function product, tha multiplies numbers after applying a
   * function f to them, but the passed function is x => x
   * @param n the number to evaluate
   * */
  def fact(n: Int) = product(x => x)(1,n)

  @main def testFact = println(fact(4))

  /**
   * An auxiliary function that apply a function f to any number in a range a to b, and then combine the results
   * applying a combination function starting in an initial value named zero
   * @param f the function to apply to the range
   * @param combine the combination function applied before apply f
   * @param zero the initial value to combine
   * @param a the left of the range
   * @param b the right of the range
   * @return an int before apply f to every value in the range and combine them with the combination function
   * */
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int =
    def recur(a: Int): Int =
      if a > b then zero
      else combine(f(a), recur(a + 1))
    recur(a)

  /**
   * Sum values before applying a function to them
   * @param f the function to apply
   * @return the final value before apply f to anyone and apply the combination function too
   * */
  def sum3(f: Int => Int) = mapReduce(f, (x,y) => x + y, 0)

  @main def testSum3 = println(sum3(fact)(1,3))

  /**
   * Multiply values before applying a function to them
   * @param f the function to apply
   * @return the final value before apply f to anyone and apply the combination function too
   * */
  def product2(f: Int => Int) = mapReduce(f, (x,y) => x * y, 1)

  @main def testProduct2 = println(product(identity)(1,2))

  /**
   * Fixed point of a function
   * A number x is called a fixed point of a function f if
   * F(x) = x
   * For some functions f we can locate the fixed points by starting with an initial estimate
   * and then by applying f in a repetitive way
   * x, f(x), f(f(x)), f(f(f(x))), ...
   * until the value does not vary anymore (or the change is sufficiently small
   *
   * */

  val tolerance =0.0001

  def abs(x: Double) = if x >= 0 then x else -x

  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
    def iterate(guess: Double): Double =
      val next = f(guess)
      if isCloseEnough(guess, next) then next
      else iterate(next)
    iterate(firstGuess)

  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2) (1.0)

  @main def testSqrtUsingFixedPoint = print(sqrt(49))

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrtImproved(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  @main def testSqrtImproved = println(sqrtImproved(49))

