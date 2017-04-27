
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    
    // computes the average from sum and count
    def avg(sum:Int, count:Int) = sum / count
    
    // tests if the cord is in picture
    def isInBound(cord: Int, exclBound: Int): Boolean = {
      cord == clamp(cord, 0, exclBound - 1)
    }
    
    require(x < src.width && y < src.height)

    //sum of colors
    var sumRed: Int = 0                  
    var sumGreen: Int = 0
    var sumBlue: Int = 0
    var sumAlpha: Int = 0
    
    // loop to compute sum  
    var count = 0                     // overall pixel count
    var xOff: Int = -radius           // offset by xAxis
    while (xOff <= +radius) {
      var yOff: Int = -radius         // offset by yAxis
      if (isInBound(x + xOff, src.width)) {
        while (yOff <= +radius) {
          if (isInBound(y + yOff, src.height)) {
            // separate out the colors
            var currentPixel: Int = src.apply(x + xOff, y + yOff)
            sumRed += red(currentPixel)
            sumGreen += green(currentPixel)
            sumBlue += blue(currentPixel)
            sumAlpha += alpha(currentPixel)
            count += 1
          }
          yOff += 1
        }
      }
      xOff += 1
    }
    // resulting averaged pixel
    rgba( avg(sumRed, count), avg(sumGreen, count), 
          avg(sumBlue, count), avg(sumAlpha, count))
  }

}
