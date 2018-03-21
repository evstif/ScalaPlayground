import calculator.Polynomial.solution

val delta = 10.0
val b = 2d
val a = 1

val ops = List[(Double, Double) => Double](_ - _, _ + _)
if (delta >= 0.0) ops.map(op => op(-b, Math.sqrt(delta) /2*a)).toSet else Set()