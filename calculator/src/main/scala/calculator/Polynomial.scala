package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()
      (bVal * bVal) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else {
        val twoA = 2 * a()
        if (delta() == 0) Set(-b() / twoA)
        else {
          val sqrtDelta = Math.sqrt(delta())
          val r1 = (-b() + sqrtDelta) / twoA
          val r2 = (-b() - sqrtDelta) / twoA
          Set(r1, r2)
        }
      }
    }
  }
}
