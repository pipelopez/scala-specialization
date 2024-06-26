package example

import example.MathematicalOperations.{cube, factorial}

import scala.annotation.tailrec

object HOF:

  /**
   * Sum all numbers between two given numbers, but applying a function to everyone before
   * @param f the function to apply to the founded number between two given numbers
   * @param a the first number
   * @param b the second number
   * @return the sum of numbers between a and b, but applying a function to everyone previously
   * */
  def sum(f: Int => Int, a: Int, b: Int): Int =
    if a > b then 0
    else f(a) + sum(f, a + 1, b)

  def sumInts(a: Int, b: Int) = sum(id, a, b)
  def sumCubes(a: Int, b: Int) = sum(cube, a, b)
  def sumFactorials(a: Int, b: Int) = sum(factorial, a, b)

  def id(x: Int): Int = x

  @main def testSum = println(sum(cube, 1, 3))

  /**
   * Anonymous functions
   * */
  val cubes = (x: Int) => x * x * x
  @main def testCubes = print(cubes(3))

  /**
   * Sum all values between two number after apply it a given function to everyone
   * @param f the function to apply to the founded number between two given numbers
   * @param a the first number
   * @param b the second number
   * @return the sum of numbers between a and b, but applying a function to everyone previously
   * */
  def recursiveSum(f: Int => Int, a: Int, b: Int): Int =
    @tailrec
    def loop(a: Int, acc: Int): Int =
      if a > b then acc
      else loop(a + 1, f(a) + acc)

    loop(a, 0)

  @main def testRecursiveSum = println(recursiveSum(cube, 1, 3))
