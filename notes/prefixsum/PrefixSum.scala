// --------------------------------------------------------------------------------
// Scan Left

def scanLeft[A](inp: array[A],
                a0: A,
                f: (A, A) => A,
                out: Array[A]): Unit = {
  out(0) = a0
  var a = a0
  var i = 0
  while (i < inp.length) {
    a = f(a, inp(i))
    i = i + 1
    out(i) = a // note, out.length == inp.length + 1
  }
}

// --------------------------------------------------------------------------------
// Define scan left using map and reduce

// Reduce a sequence using some function, i.e. sum
def reduceSeg1[A](inp: Array[A],
                  left: Int,
                  right: Int,
                  a0: A,
                  f:(A, A) => A): A

// Tranform elements of an array
def mapSeg[A, B](inp: Array[A],
                 left: Int,
                 right: Int,
                 fi: (Int, A) => B,
                 out: Array[B]): Unit

// New scanLeft (inefficient)
def scanLeft[A](inp: Array[A],
                a0: A,
                f: (A, A) => A,
                out: Array[A]) = {
  // Create a function that reduces up to the ith position (this will be repetitive)
  val fi = { (i: Int, v: A) => reduceSeq1(inp, 0, i, a0, f) }
  mapSeq(inp, 0, inp.length, fi, out)
  // Manually do the last element in the output
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))

}
// --------------------------------------------------------------------------------
// Reuse intermediate results

// Trees storing input collection values in leaves only
sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

// Trees storing intermediate values (res) values in every node
// res could be, for example, partial sums
sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
                      override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

// Transform Tree into TreeRes, containing partial results (i.e. partial sums)
def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case node(l, r) => {
    val (tL, tR) = (reduceRs(l, f), reduceRes(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

// Parallel version of reduceRes - create TreeRes containing partial results
def upsweep[A](t: Tree[A]., f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(v) => LeafRes(v)
  case Node(l, r) => {
    val (tL, tR) = parallel(up(l, f), upsweep(r, f))
    NodeRes(tL, f(tL.res, tR.res), tR)
  }
}

// Create Tree containing resulting prefix sum collection at the leaves
def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(downsweep[A](l, a0, f),
                            downsweep[A](r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

// Use upsweep and downsweep to run scanLeft in parallel
def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  val tRes = upsweep(t, f)
  val scan1 = downsweep(tRes, a0, f)
  prepend(a0, scan1)
}

def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
  case Leaf(v) => Node(Leaf(x), Leaf(v))
  case Node(l, r) => Node(prepend(x, l), r)
}

// --------------------------------------------------------------------------------
// Arrays in leaves instead of individual elements (more efficient)

sealed abstract class TreeResA[A] { val res: A }
case class Leaf[A](from: int, to: int,
                   override val res: A) extends TreeResA[A]
case class Node[A](l: TreeResA[A],
                   override val res: A,
                   r: TreeResA[A]) extends TreeResA[A]


def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
  if (to - from < threshold)
    Leaf(from, to, reduceSeg1(inp, from + 1, to, inp(from), f))
  else {
    val mid = from + (to - from) / 2
    val (tL, tR) = parallel(upsweep(inp, from, mid, f),
                            upsweep(inp, mid, to, f))
    Node(tL, f(tL.res, tR.res), tR)
  }
}

TODO: downsweep[A]
https://www.coursera.org/learn/parprog1/lecture/934xD/parallel-scan-prefix-sum-operation
    21:33

