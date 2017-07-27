package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  // My tests
  test("parLineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 2)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  // test("lineOfSight should correctly handle an array of size 17") {
  //   val output = new Array[Float](17)
  //   lineOfSight(Array[Float](0, 2, 3, 4, 5, 6, 7, 8f, 9f, 10f, 11f, 12f, 13, 14, 18, 16, 17), output)
  //   assert(output.toList == List(1f, 2, 3, 4, 5, 6, 7, 8f, 9f, 10f, 11f, 12f, 13, 14, 18, 18, 18))
  // }

  // test("parLineOfSight should correctly handle an array of size 17") {
  //   val output = new Array[Float](17)
  //   parLineOfSight(Array[Float](1f, 2, 3, 4, 5, 6, 7, 8f, 9f, 10f, 11f, 12f, 13, 14, 18, 16, 17), output, 1)
  //   assert(output.toList == List(1f, 2, 3, 4, 5, 6, 7, 8f, 9f, 10f, 11f, 12f, 13, 14, 18, 18, 18))
  // }

}

