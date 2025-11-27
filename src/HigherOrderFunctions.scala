def integracion(f: Double => Double, a: Double, b: Double): Double =
  val xm = (a + b) / 2
  ((b - a) / 6) * (f(a) + 4 * f(xm) + f(b))

def error(valorReal: Double, valorAprox: Double): Double =
  math.abs(valorReal - valorAprox)

@main def run() =
  val i1 = integracion(x => -x*x + 8*x - 12, 1, 3)
  val e1 = error(7.33, i1)

  val i2 = integracion(x => 3*x*x, 1, 2)
  val e2 = error(8, i2)

  val i3 = integracion(x => x + 2*x*x - x*x*x + 5*x*x*x*x, -1, 2)
  val e3 = error(3.333, i3)

  val i4 = integracion(x => (2*x + 1) / (x*x + x), 1, 3)
  val e4 = error(1.09861, i4)

  val i5 = integracion(x => math.exp(x), 1, 2)
  val e5 = error(1.71828, i5)

  val i6 = integracion(x => 1 / math.sqrt(x), 0, 1)
  val e6 = error(0.828427, i6)

  val i7 = integracion(x => 1 / (1 + x*x), 0, 1)
  val e7 = error(0.785398, i7)

  println("Integral 1: " + i1 + " Error: " + e1)
  println("Integral 2: " + i2 + " Error: " + e2)
  println("Integral 3: " + i3 + " Error: " + e3)
  println("Integral 4: " + i4 + " Error: " + e4)
  println("Integral 5: " + i5 + " Error: " + e5)
  println("Integral 6: " + i6 + " Error: " + e6)
  println("Integral 7: " + i7 + " Error: " + e7)
