import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {

    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = {
      if (mass == 0)
        centerX
      else
        Seq(nw, ne, sw, se).map(q => q.massX * q.mass).sum / mass
    }
    val massY: Float = {
      if (mass == 0)
        centerY
      else
        Seq(nw, ne, sw, se).map(q => q.massY * q.mass).sum / mass
    }
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (b.x < centerX) {
        if (b.y < centerY) Fork(nw.insert(b), ne, sw, se)
        else Fork(nw, ne, sw.insert(b), se)
      } else {
        if (b.y < centerY) Fork(nw, ne.insert(b), sw, se)
        else Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
      extends Quad {
    val mass = bodies.map(_.mass).sum
    val massX = bodies.map(b => b.mass * b.x).sum / mass
    val massY = bodies.map(b => b.mass * b.y).sum / mass
    val total: Int = bodies.length
    def insert(b: Body): Quad = size > minimumSize match {
      case true => {
        val halfSize: Float = size / 2
        val offset: Float = size / 4
        val newFork = Fork(Empty(centerX - offset, centerY - offset, halfSize),
                           Empty(centerX + offset, centerY - offset, halfSize),
                           Empty(centerX - offset, centerY + offset, halfSize),
                           Empty(centerX + offset, centerY + offset, halfSize))
        val newBodies = bodies :+ b
        newBodies.foldLeft(newFork){case (agg: Fork, x: Body) => agg.insert(x)}
      }
      case false => {
        Leaf(centerX, centerY, size, bodies :+ b)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        // see if node is far enough from the body,
        // or recursion is needed
        case Empty(_, _, _) => {}
          // no force
        case Leaf(_, _, _, bodies) => bodies.map(b => addForce(b.mass, b.x, b.y))
          // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          quad.size / distance(x, y, quad.massX, quad.massY) < theta match {
            case true => addForce(quad.mass, quad.massX, quad.massY)
            case false => {
              traverse(nw)
              traverse(ne)
              traverse(sw)
              traverse(se)
            }
          }
          // Seq(nw, ne, sw, se).map(
          // q => q.size / distance(x, y, q.centerX, q.centerY) < theta match {
          //   case true => addForce(q.mass, q.massX, q.massY)
          //   case false => traverse(q)
          // })
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val boundedX: Float = Math.min(Math.max(boundaries.minX, b.x), boundaries.maxX - 1)
      val boundedY: Float = Math.min(Math.max(boundaries.minY, b.y), boundaries.maxY - 1)
      val tX: Int = ((boundedX - boundaries.minX) / boundaries.width * sectorPrecision).toInt
      val tY: Int = ((boundedY - boundaries.minY) / boundaries.height * sectorPrecision).toInt
      val cb: ConcBuffer[Body] = apply(tX, tY)
      cb += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      var i = 0
      val newMat = this.matrix.zip(that.matrix).map{case (cb1, cb2) => cb1.combine(cb2)}
      while (i < newMat.length) {
        this.matrix(i) = newMat(i)
        i += 1
      }
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
