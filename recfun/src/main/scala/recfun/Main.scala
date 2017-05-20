package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def isBalanced(chars: List[Char], level: Int): Boolean = {
      if (level < 0) false
      else if (chars.isEmpty) level == 0
      else if (chars.head == '(') isBalanced(chars.tail, level + 1)
      else if (chars.head == ')') isBalanced(chars.tail, level - 1)
      else isBalanced(chars.tail, level)
    }
    isBalanced(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (money >= coins.head) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    else countChange(money, coins.tail)
  }
}
