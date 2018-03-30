
import common._

import scala.reflect.internal.util.Statistics.Counter

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

    override def toString: String = if (data.forall(p => p == 0)) "empty" else super.toString
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//    val xs = x - radius
//    val ys = y - radius
//    val range = radius * 2 + 1
//    //println(src)
//    //println(s"x - $x, y - $y, xs - $xs, ys - $ys, radius - $radius")
//    val pixelsByChannel = for(xi <- xs until range by 1;
//                              yi <- ys until range by 1;
//                              rgba = src.apply(clamp(xi, 0, src.width - 1), clamp(yi, 0, src.height - 1)))
//      yield (red(rgba), green(rgba), blue(rgba), alpha(rgba))
//    //println(pixelsByChannel)
//    val sumByChannel = pixelsByChannel.distinct match {
//      case Seq() => (0,0,0,0)
//      case _ => pixelsByChannel.reduce[(Int, Int, Int, Int)]((a1, a2)
//      => (a1._1 + a2._1, a1._2 + a2._2, a1._3 + a2._3, a1._4 + a2._4))
//    }
//    val count = range * range
//
//    rgba(sumByChannel._1/count, sumByChannel._2/count,sumByChannel._3/count, sumByChannel._4/count)
//  }
  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val pixels = {
      for (
        i <- -radius to radius;
        j <- -radius to radius
      ) yield (scalashop.clamp(x + i, 0, src.width - 1), scalashop.clamp(y + j, 0, src.height - 1))
    }.distinct.map({
      case (x, y) =>
        val pixel = src(x, y)
        (red(pixel), green(pixel), blue(pixel), alpha(pixel))
    })

    rgba(
      pixels.map(_._1).sum / pixels.length,
      pixels.map(_._2).sum / pixels.length,
      pixels.map(_._3).sum / pixels.length,
      pixels.map(_._4).sum / pixels.length
    )
  }

  def intervals(number: Int, count: Int): Iterator[(Int,Int)] = {
    var split = (0 until number by (number/count)).toArray
    if (split.length == count && ((split.length - split.last) < split.length/count/2))
      split = split :+ number
    else
      split(split.length - 1) = number

    split.toVector.sliding(2).map(v => (v.head, v.last))
  }

}
