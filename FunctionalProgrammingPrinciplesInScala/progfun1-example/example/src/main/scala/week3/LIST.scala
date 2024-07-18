package week3

import scala.annotation.tailrec

trait LIST[T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]

class CONS[T](val head: T, val tail: LIST[T]) extends LIST[T]:
  def isEmpty: Boolean = false

class NIL[T] extends LIST[T]:
  def isEmpty: Boolean = true
  def head: T = throw new NoSuchElementException("Nil.head")
  def tail: LIST[T] = throw new NoSuchElementException("Nil.tail")

@tailrec
def nth[T](xs: LIST[T], n: Int): T =
  if xs.isEmpty then throw IndexOutOfBoundsException()
  else if n == 0 then xs.head
  else nth(xs.tail, n - 1)

val linkedList: CONS[Int] = CONS(1, CONS(2, CONS(3, NIL())))

@main def testLinkedList = println(nth(linkedList, 2))
