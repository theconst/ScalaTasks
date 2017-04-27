import common._
import barneshut.conctrees._


/* this package contains common data structures for Barnes-Hut simulation */


// package object declarations means that everything declared will be visible from the package
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

  
  /** 
   *  Quad tree contains either
   *  1. Fork
   *  2. Leaf
   *  3. Empty node
   *  
   *  Quad tree partitions the two-dimensional space
   *  
   */
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

  
  /**
   * represents a node without bodies in it
   * centerX - x-center
   * centerY - y-center
   * size
   */
  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, List(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    
    val centerX: Float = (nw.centerX + ne.centerX + sw.centerX + se.centerX) / 4
    val centerY: Float = (nw.centerY + ne.centerY + sw.centerY + se.centerY) / 4
    val size: Float = nw.size + ne.size                           //+ sw.size + se.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else 
                            1 / mass * (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX)                   
    val massY: Float = if (mass == 0) centerY else 
                            1 / mass * (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY)
    val total: Int = nw.total + ne.total + sw.total + se.total
    

    def insert(b: Body): Fork = {
      def contains(q: Quad, b: Body): Boolean = {
        (((q.centerX - size / 2 <= b.x) && (b.x <= q.centerX + size / 2)) 
                                        && ((q.centerY - size / 2 <= b.y) && (b.y <= q.centerY + size / 2)))
      }
      
      if (contains(nw, b)) {
        Fork(nw insert b, ne, sw, se)
      } else if (contains(ne, b)) {
        Fork(nw, ne insert b, sw, se)
      } else if (contains(sw, b)) {
        Fork(nw, ne, sw insert b, se)
      } else {                         //if (contains(se, b))
        Fork(nw, ne, sw, se insert b)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {
    
    // use more cute solution later
    private val totalMass = bodies.map(_.mass).sum
    private val totalMassX = bodies.map(b => (b.mass * b.x)).sum / totalMass
    private val totalMassY = bodies.map(b => (b.mass * b.y)).sum / totalMass
    
    val (mass, massX, massY) = (totalMass, totalMassX, totalMassY)
    val total: Int = bodies.length
    
    //?
    def insert(b: Body): Quad = if (size > minimumSize) {

      val newSize = size / 2.0f
      val offset = newSize / 2.0f
      val newNode = new Fork(
           Empty(centerX - offset, centerY - offset, newSize), //nw
           Empty(centerX + offset, centerY - offset, newSize), //ne
           Empty(centerX - offset, centerY + offset, newSize), //sw
           Empty(centerX + offset, centerY + offset, newSize)) //se
      
      bodies.foldLeft(newNode insert b)((s, x) => s insert x)
    } else {
      new Leaf(centerX, centerY, size, bodies :+ b)
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
      
      // this method relies on side effect
      def traverse(quad: Quad): Unit = {
        def dist(q: Quad) = distance(x, y, quad.massX, quad.massY)
        
        def isFarEnough(quad: Quad) = (quad.size / dist(quad)) < theta
        
        (quad: Quad) match {
            case Empty(_, _, _) =>
            // no force - just do nothing
            case Leaf(_, _, _, bodies) =>
            // add force contribution of each body by calling addForce
                bodies.foreach(b => addForce(b.mass, b.x, b.y))
            case Fork(nw, ne, sw, se) =>
            // see if node is far enough from the body => opaque, if no use recursion
              if (isFarEnough(quad)) {
                 addForce(quad.mass, quad.massX, quad.massY)  
              } else {
                 traverse(nw); traverse(ne); traverse(sw); traverse(se) 
              }
        }
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

  
  //! combiner should be totally reimplemented
  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize: Float = boundaries.size / sectorPrecision.floatValue
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
     
        // floor the resulting indexes of the array matrix; 
        // then determine the closest point within the boundaries;
        val x: Int = Math.max(0, Math.round(Math.min(boundaries.maxX, Math.floor(b.x / sectorSize).intValue)))
        val y: Int = Math.max(0, Math.round(Math.min(boundaries.maxY, Math.floor(b.y / sectorSize).intValue)))
        
        // add item to the ConcBuffer[T]
        this(x, y) += b      //apply(x,y) => get the ConcBuffer => add body
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    /** 
     * Calls combine operation for each corresponding ConcBuffer of the sector matrix
     */
    def combine(that: SectorMatrix): SectorMatrix = {
        val result: SectorMatrix = new SectorMatrix(this.boundaries, this.sectorPrecision)
        val thisMatIt = this.matrix.iterator
        val thatMatIt = that.matrix.iterator
        
        for (i <- (0 until result.matrix.length)) {
          result.matrix(i) = thisMatIt.next combine thatMatIt.next
        }    
        result
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
