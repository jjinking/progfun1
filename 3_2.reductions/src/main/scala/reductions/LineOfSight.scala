package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    output(0) = 0
    var i = 1
    while (i < input.length) {
      output(i) = max(input(i) / i, output(i - 1))
      i += 1
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], startPos: Int, endPos: Int): Float = startPos < endPos match {
    case false => -1
    case true => {

      var idx: Int = startPos
      var curMax: Float = 0
      while (idx < endPos) {
        curMax = max(curMax, input(idx) / max(1, idx))
        idx += 1
      }
      curMax
    }
      // input.slice(startPos, endPos)
      //   .zip(startPos until endPos)
      //   .map{case (y, x) => y.toFloat / max(1, x)}
      //   .max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], startPos: Int, endPos: Int,
              threshold: Int): Tree = (endPos - startPos) <= threshold match {
    case true => Leaf(startPos, endPos, upsweepSequential(input, startPos, endPos))
    case false => {
      val mid = (startPos + endPos) / 2
      if (mid > startPos && endPos > mid) {
        val (tL, tR) = parallel(upsweep(input, startPos, mid, threshold),
                                upsweep(input, mid, endPos, threshold))
        Node(tL, tR)
      } else if (mid > startPos) {
        Leaf(startPos, endPos, upsweepSequential(input, startPos, mid))
      } else {
        Leaf(mid, endPos, upsweepSequential(input, mid, endPos))
      }
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, startPos: Int, endPos: Int): Unit = {
    if (startPos < endPos) {
      var i: Int = startPos
      var prev: Float = startingAngle
      while (i < endPos) {
        output(i) = max(input(i) / max(i, 1), prev)
        prev = output(i)
        i += 1
      }
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = tree match {
    case Leaf(startPos, endPos, maxPrev) => {
      downsweepSequential(input, output, startingAngle, startPos, endPos)
    }
    case Node(tL, tR) => {
      val (_, _) = parallel(
        downsweep(input, output, startingAngle, tL),
        downsweep(input, output, max(startingAngle, tL.maxPrevious), tR)
      )
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
                     threshold: Int): Unit = {
    val t = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0, t)
  }
}
