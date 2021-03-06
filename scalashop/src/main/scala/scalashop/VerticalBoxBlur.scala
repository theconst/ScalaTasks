package scalashop

import org.scalameter._
import java.util.concurrent.ForkJoinTask
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    //
    var col: Int = from
    var row: Int = 0;
    while (col < end) {
      row = 0
      while (row < src.height) {
        dst.update(col, row, boxBlurKernel(src, col, row, radius))
        row += 1
      }
      col += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val piece: Int = if (numTasks > src.width) 1 else src.width / numTasks
    
    def loop(covered: Int, tasks : List[ForkJoinTask[Unit]]) {
      if (covered + piece >= src.width) {
        (task(blur(src, dst, covered, src.width, radius)) :: tasks).foreach((t) => t.join())
      } else {
        loop(covered + piece,
            task(blur(src, dst, covered, covered + piece, radius)) :: tasks)
      }
    }
    loop(0, Nil)
  }
}
