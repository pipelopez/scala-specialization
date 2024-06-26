package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   * 1            (c=0 , r= whatever) => 1;       if c == 0 => 1
   * 1  1         (c=0 , r= whatever) => 1 ; (c=1 , r=1) => 1;  if c== 0 || c == r => 1
   * 1  2  1      (c=1 , r = 2) => (c=0, r=1) + (c=1, r=1) => (c-1, r-1) + (c, r-1) => else pascal(c-1, r-1) + pascal (c, r-1)
   * 1  3  3  1
   * 1  4  6  4  1
   * 1  5  10 10 5  1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   * :-) FirstIteration: nonEmpty, head=:, tail = List(-, )) => 0;
   * -)  SecondOne: nonEmpty, head=-, tail = List()) => 0;
   * )   ThirdOne: nonEmpty, head=), tail=List.empty => -1 then false;
   * */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balanceHelper(chars: List[Char], balanceCount: Int): Boolean = {
      if (chars.isEmpty) then balanceCount == 0
      else {
        val head = chars.head
        val newBalanceCount =
          if (head == '(') then balanceCount + 1
          else if (head == ')') then balanceCount - 1
          else balanceCount

        if (newBalanceCount < 0) then false
        else balanceHelper(chars.tail, newBalanceCount)
      }
    }

    balanceHelper(chars, 0)

  /**
   * Exercise 3
   * 4,List(1,2)  : 4!0, 4>0, nonEmpty => (4-1, List(1,2)) + (4, List(2))
   * 3,List(1,2)  : 3!0, 3>0, nonEmpty => (3-1, List(1,2)) + (3, List(2))
   * 2,List(1,2)  : 2!0, 2>0, nonEmpty => (2-1, List(1,2)) + (2, List(2))
   * 1,List(1,2)  : 1!0, 1>0, nonEmpty => (1-1, List(1,2)) + (1, List(2))
   * 0,List(1,2)  : 0=0 => 1
   * 1,List(2)    : 1!0, 1>0, nonEmpty => (1-2, List(2))   + (1, List.empty)
   * -1,List(2)   : -1!0, -1<0, nonEmpty => 0
   * 1,List.empty : empty => 0
   * 2,List(2)    : 2!0, 2>0, nonEmpty => (2-2, List(2)) + (2, List.empty)
   * 0,List(2)    : 0=0 => 1
   * 2,List.empty : empty => 0
   * 3,List(2)    : (3-2, List(2)) + (3, List.empty)
   * 1,List(2)    : (1-2, List(2)) + (1, List.empty)
   * -1,List(2)   : -1<0 => 0
   * 1,List.empty : empty => 0
   * 3,List.empty : empty => 0
   * 4,List(2)    : (4-2, List(2)) + (4, List.empty)
   * 2,List(2)    : (2-2, List(2)) + (2, List.empty)
   * 0,List(2)    : 0=0 => 1
   * 2,List.empty : empty => 0
   * 4,List.empty : empty => 0
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def countChangeHelper(money: Int, coins: List[Int]): Int = {
      if (money == 0) then 1
      else if (money < 0 || coins.isEmpty) then 0
      else countChangeHelper(money - coins.head, coins) + countChangeHelper(money, coins.tail)
    }

    countChangeHelper(money, coins)
