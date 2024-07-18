package example

object Lists:

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = if (xs.isEmpty) 0 else xs.head + sum(xs.tail)

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    if (xs.isEmpty) throw new NoSuchElementException("Empty list")
    else {
      def maxHelper(currentBiggest: Int, restOfTheList: List[Int]): Int = {
        if (restOfTheList.isEmpty) currentBiggest
        else maxHelper(if (restOfTheList.head > currentBiggest) restOfTheList.head else currentBiggest, restOfTheList.tail)
      }
      maxHelper(xs.head, xs.tail)
    }
  }

  /**
   * Remueve el elemento en la posición dada para la lista
   * @param n es el index del dato a remover
   * @param xs es la lista de la cual se va a remover el dato
   * @return una lista nueva sin el dato que se removió
  */
  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
    case Nil => Nil
    case y :: ys =>
      if n == 0 then ys
      else y :: removeAt(n-1, ys)

  val r: List[Char] = "abcd".toList

  @main def testRemoveAt = println(removeAt(2, r))

  /**
   * Aplana la lista de listas de objetos en una lista de objetos
   * @param xs es la lista a aplanar
   * @return una lista nueva
   */

  def flatten(xs: Any): List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ++ flatten(ys)
    case _ => xs :: Nil

    val y = List(List(1,3), List(2), 4, 5, List(6))

  @main def testFlatten = println(flatten(y))

  /**
   * devuelve una lista con las listas de elementos que son iguales dentro de una lista
   * @param xs es la lista a analizar
   * @return una lista con las listas de elementos que son iguales
   * */
  def pack[T](xs: List[T]): List[List[T]] = xs match
    case Nil => Nil
    case x :: xs1 =>
      val (more, rest) = xs1.span(y => y == x)
      (x :: more) :: pack(rest)

  val elements: List[Char] = "aaabcca".toList

  @main def testPack = println(pack(elements))

  /**
   * devuelve un listado de tuplas con el elemento y la cantidad de veces que se repite en la lista original
   * @param xs es la lista a analizar
   * @return un listado de tuplas
   * */
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(x => (x.head, x.length))

  @main def testEncode = println(encode(elements))

  /**
   * es un mapping apoyándose en la función foldRight
   * @param xs es una lista
   * @param f es la función que se aplicará sobre la lista
   * @return una lista con el resultado de aplicar la función f a cada elemento de xs
   * */
  def mapUsingFoldRight[T, U](xs: List[T], f: T => U): List[U] =
    xs.foldRight(List[U]())((y, ys) => f(y) :: ys)


  /**
   * es un lenght apoyándose en la función foldRight
   * @param xs es una lista
   * @return la longitud de una lista
   * */
  def lengthUsingFoldRight[T](xs: List[T]): Int =
    xs.foldRight(0)((y, n) => n + 1)

  val numbers: List[Char] = "12345678901234580".toList

  val resMapping = mapUsingFoldRight(numbers, (el: Char) => el.toInt)
  val mapNormal = numbers.map(_.toInt)
  val resLenght = lengthUsingFoldRight(numbers)

  @main def testMapAndLenght = println(s"$resMapping \n$mapNormal \n$resLenght ")



