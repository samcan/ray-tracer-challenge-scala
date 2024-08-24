// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class CanvasSuite extends munit.FunSuite {
  test("Creating a canvas") {
    val rows = 10
    val cols = 20
    val canvas = MakeCanvas(rows, cols)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } assertEquals(PixelAt(canvas, i, j), Some(Color(0, 0, 0)))
  }

  test("Test PixelAt returns None for out of range row") {
    val rows = 10
    val cols = 20
    val canvas = MakeCanvas(rows, cols)

    assertEquals(PixelAt(canvas, rows, 0), None)
  }

  test("Test PixelAt returns None for out of range col") {
    val rows = 10
    val cols = 20
    val canvas = MakeCanvas(rows, cols)

    assertEquals(PixelAt(canvas, 0, cols), None)
  }

  test("WritePixel sets a pixel on the canvas") {
    val rows = 10
    val cols = 20
    val canvas = MakeCanvas(rows, cols)

    // write new color at specific location
    val row = 2
    val col = 3
    val redColor = Color(1, 0, 0)
    val outputCanvas = WritePixel(canvas, row, col, redColor)

    assertEquals(PixelAt(outputCanvas, row, col), Some(redColor))
  }

  // TODO Test WritePixel with out-of-bounds indices
}
