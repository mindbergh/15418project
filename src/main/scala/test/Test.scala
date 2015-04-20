@cuSparkCode
object Test {
  val width = 1200
  val height = 800
  val maxIterattions = 256

  val x0 = -2.0
  val x1 = 1.0
  val y0 = -1.0
  val y1 = 1.0

  val dx = (x1 - x0) / width
  val dy = (y1 - y0) / width

  def mandel(c_re: Double, c_im: Double, count: Int): Int = {
    var z_re = c_re
    var z_im = c_im
    var i = -1
    while (z_re * z_re + z_im * z_im <= 4) {
      i += 1
      val new_re = z_re * z_re - z_im * z_im
      val new_im = 2 * z_re * z_im
      z_re = c_re + new_re
      z_im = c_im + new_im
    }
    i
  }

  val out = for {
    index <- 1 to width * height
    i = width / index
    j = width % index
    x = x0 + i * dx
    y = y0 + j * dy
  } yield mandel(x, y, maxIterattions)


  println(out)
}