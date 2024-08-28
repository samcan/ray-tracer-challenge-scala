import java.io._
import org.scalactic._
import org.scalactic.TripleEquals._

val EPSILON = 0.00001

implicit val doubleEquality: Equality[Double] =
  TolerantNumerics.tolerantDoubleEquality(EPSILON)

implicit val matrixEquality: Equality[IndexedSeq[IndexedSeq[Double]]] =
  new Equality[IndexedSeq[IndexedSeq[Double]]] {
    def areEqual(
        a: IndexedSeq[IndexedSeq[Double]],
        b: Any
    ): Boolean = {
      b match {
        case m: IndexedSeq[IndexedSeq[Double]] =>
          a.flatten
            .zip(m.flatten)
            .map((value1, value2) => value1 === value2)
            .foldLeft(true)((result, b) => result && b)
        case _ => false
      }
    }
  }

implicit val tupleEquality: Equality[IndexedSeq[Double]] =
  new Equality[IndexedSeq[Double]] {
    def areEqual(
        a: IndexedSeq[Double],
        b: Any
    ): Boolean = {
      b match {
        case m: IndexedSeq[Double] =>
          a.zip(m)
            .map((value1, value2) => value1 === value2)
            .foldLeft(true)((result, b) => result && b)
        case _ => false
      }
    }
  }

@main def RayTracer(): Unit = {
  // size of the canvas as a square
  val canvasPixels = 200

  val red = Color(1, 0, 0)
  val material = Material().copy(color = Color(1, 0.2, 1))
  val shape = Sphere().copy(material = material)

  val eye = point(0, 0, -5)

  // define light source
  val light = PointLight(point(-10, 10, -10), Color(1, 1, 1))

  val wallSize = 7.0
  val wallZ = 10.0
  val pixelSize = wallSize / canvasPixels
  val half = wallSize / 2
  val canvas = MakeCanvas(canvasPixels, canvasPixels)

  // val pixels = canvas.zipWithIndex.map((row, y) =>
  //   TraceCanvasRow(
  //     row,
  //     y,
  //     red,
  //     shape,
  //     rayOrigin,
  //     half,
  //     pixelSize,
  //     wallZ
  //   )
  // )

  for {
    y <- 0 to canvasPixels - 1
    x <- 0 to canvasPixels - 1
  } yield CalculatePixel(
    canvas,
    x,
    y,
    GetWorldX(half, pixelSize, x),
    GetWorldY(half, pixelSize, y),
    wallZ,
    light,
    shape,
    eye
  )

  // save the canvas as a ppm file
  val pw = new PrintWriter(new File("output.ppm"))
  CanvasToPPM(canvas).map(pw.write)
  pw.close
}

def GetWorldX(half: Double, pixelSize: Double, x: Int): Double = {
  -half + pixelSize * x
}

def GetWorldY(half: Double, pixelSize: Double, y: Int): Double = {
  half - pixelSize * y
}

def CalculatePixel(
    canvas: Array[Array[Color]],
    x: Int,
    y: Int,
    wx: Double,
    wy: Double,
    wz: Double,
    light: PointLight,
    shape: Sphere,
    rayOrigin: Tuple
): Unit = {
  val position = point(wx, wy, wz)
  val r = Ray(rayOrigin, normalize(position - rayOrigin))
  val xs = Intersect(shape, r)
  val hit = Hit(xs)

  if hit.isDefined then
    val point = Position(r, hit.get.t)
    val normal = Normal(hit.get.obj, point)
    val eye = negate(r.direction)
    val color = Lighting(hit.get.obj.material, light, point, eye, normal.get)
    SetPixel(canvas, x, y, color)
}

// def TraceCanvasRow(
//     row: IndexedSeq[Color],
//     y: Int,
//     color: Color,
//     shape: Sphere,
//     rayOrigin: Tuple,
//     half: Double,
//     pixelSize: Double,
//     wallZ: Double
// ): Seq[(Int, Int, Color)] = {
//   val world =
//     row.zipWithIndex.map((c, x) =>
//       (x, y, GetWorldX(half, pixelSize, x), GetWorldY(half, pixelSize, y))
//     )
//   val positions = world.map((x, y, wx, wy) => (x, y, point(wx, wy, wallZ)))
//   val rays =
//     positions.map((x, y, p) => (x, y, Ray(rayOrigin, normalize(p - rayOrigin))))
//   val intersections = rays.map((x, y, r) => (x, y, Intersect(shape, r)))
//   val pixels = intersections
//     .map((x, y, xs) => (x, y, Hit(xs)))
//     .filter((x, y, h) => h.isDefined)
//     .map((x, y, h) => (x, y, color))
//   pixels
// }
