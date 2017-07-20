package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val r = scala.util.Random

  // lazy val genMap: Gen[Map[Int,Int]] = oneOf(
  //   const(Map.empty[Int,Int]),
  //   for {
  //     k <- arbitrary[Int]
  //     v <- arbitrary[Int]
  //     m <- oneOf(const(Map.empty[Int,Int]), genMap)
  //   } yield m.updated(k, v)
  // )

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (h: H) =>
    val x = r.nextInt
    val y = r.nextInt
    val h = insert(y, insert(x, empty))
    findMin(h) == Math.min(x, y)
  }

  property("min3") = forAll { (h: H) =>
    val h = insert(r.nextInt, empty)
    isEmpty(deleteMin(h))
  }

  def getSorted(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: getSorted(deleteMin(h))
  }

  property("sorted") = forAll { (h: H) =>
    val ordered = getSorted(h)
    ordered.sorted == ordered
  }


  // def getMin(h: H): Option[A] = {
  //   if (isEmpty(h)) None
  //   else Some(findMin(h))
  // }

  // def getMin(h1: H, h2: H): Option[A] = {
  //   getMin(h1) match {
  //     case None => getMin(h2)
  //     case other => other
  //   }
  // }

  property("meld") = forAll { (h1: H) =>
    forAll { (h2: H) =>
      {
        if (isEmpty(h1) && isEmpty(h2)) true
        else {
          val meldedMin = findMin(meld(h1, h2))
          if (isEmpty(h1)) meldedMin == findMin(h2)
          else if (isEmpty(h2)) meldedMin == findMin(h1)
          else meldedMin == Math.min(findMin(h1), findMin(h2))
        }
      }
    }
  }

  property("meld2") = forAll { (h1: H) =>
    forAll { (h2: H) =>
      {
        (getSorted(h1) ::: getSorted(h2)).toSet.size == getSorted(meld(h1, h2)).toSet.size
      }
    }
  }
}
