// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class CanvasSuite extends munit.FunSuite {
  test("Creating a canvas") {
    val width = 20
    val height = 10
    val canvas = MakeCanvas(width, height)

    for {
      i <- 0 until width
      j <- 0 until height
    } assertEquals(PixelAt(canvas, i, j), Some(Color(0, 0, 0)))
  }

  test("Creating a canvas with a specified color") {
    val width = 5
    val height = 3
    val color = Color(1, 0, 0)
    val canvas = MakeCanvas(color)(width, height)

    for {
      i <- 0 until width
      j <- 0 until height
    } assertEquals(PixelAt(canvas, i, j), Some(color))
  }

  test("Test PixelAt returns None for out of range col") {
    val width = 20
    val height = 10
    val canvas = MakeCanvas(width, height)

    assertEquals(PixelAt(canvas, width, 0), None)
  }

  test("Test PixelAt returns None for out of range row") {
    val width = 20
    val height = 10
    val canvas = MakeCanvas(width, height)

    assertEquals(PixelAt(canvas, 0, height), None)
  }

  test("SetPixel sets a pixel on the canvas") {
    val width = 20
    val height = 10
    val canvas = MakeCanvas(width, height)

    // write new color at specific location
    val x = 2
    val y = 3
    val redColor = Color(1, 0, 0)
    val outputCanvas = SetPixel(canvas, x, y, redColor)

    assertEquals(PixelAt(outputCanvas, x, y), Some(redColor))
  }

  // TODO Test SetPixel with out-of-bounds indices
}
