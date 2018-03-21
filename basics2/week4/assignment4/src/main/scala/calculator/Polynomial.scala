
package calculator

object Polynomial {
  
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal(Math.pow(b(), 2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val ops = List[(Double, Double) => Double](_ - _, _ + _)
    new Signal( if (delta() > 0.0) ops.map(op => op(-b(), Math.sqrt(delta())) / (2 * a())).toSet else Set() )
  }
}
