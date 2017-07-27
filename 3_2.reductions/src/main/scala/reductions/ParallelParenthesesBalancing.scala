package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 1000
    val chars = new Array[Char](length)
    val threshold = 10
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def isBalanced(chars: Array[Char], level: Int): Boolean = {
      if (level < 0) false
      else if (chars.isEmpty) level == 0
      else if (chars.head == '(') isBalanced(chars.tail, level + 1)
      else if (chars.head == ')') isBalanced(chars.tail, level - 1)
      else isBalanced(chars.tail, level)
    }
    isBalanced(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, opening: Int, closing: Int): (Int, Int)  = {
      if (idx >= until) {
        (opening, closing)
      } else if (chars(idx) == '(') {
        traverse(idx + 1, until, opening + 1, closing)
      } else if (chars(idx) == ')') {
        if (opening > 0) {
          // Close the parens that opened in this segment
          traverse(idx + 1, until, opening - 1, closing)
        } else {
          traverse(idx + 1, until, opening, closing + 1)
        }
      } else {
        // Not ( or )
        traverse(idx + 1, until, opening, closing)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2 
        val ((open1, close1), (open2, close2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (open1 > close2) (open1 + open2 - close2, close1)
        else (open2, close1 + close2 - open1)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
