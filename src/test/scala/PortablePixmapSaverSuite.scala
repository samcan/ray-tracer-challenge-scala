class PortablePixmapSaverSuite extends munit.FunSuite {
  test("Constructing the PPM header") {
    val width = 20
    val height = 10
    val c = MakeCanvas(width, height)

    val output = CanvasToPPM(c)

    assert(output.length >= 3)
    assertEquals(output.apply(0), "P3\n")
    assertEquals(output.apply(1), s"$height $width\n")
    assertEquals(output.apply(2), "255\n")
  }

  test("Constructing the PPM pixel data") {
    val width = 5
    val height = 3
    val c = MakeCanvas(width, height)
    val outputCanvas = SetPixel(c, 0, 0, Color(1.5, 0, 0))
    val outputCanvas2 = SetPixel(outputCanvas, 2, 1, Color(0, 0.5, 0))
    val outputCanvas3 = SetPixel(outputCanvas2, 4, 2, Color(-0.5, 0, 1))

    val output = CanvasToPPM(outputCanvas3)
    assert(output.length >= 6)
    assertEquals(output.apply(3), "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n")
    assertEquals(output.apply(4), "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n")
    assertEquals(output.apply(5), "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n")
  }

  test("Writing a color: RED") {
    val c = Color(1, 0, 0)
    assertEquals(WritePixel(c), "255 0 0")
  }

  test("Writing a color: GREEN") {
    val c = Color(0, 1, 0)
    assertEquals(WritePixel(c), "0 255 0")
  }

  test("Writing a color: BLUE") {
    val c = Color(0, 0, 1)
    assertEquals(WritePixel(c), "0 0 255")
  }

  test("Writing a color (clamped)") {
    val c = Color(-1.5, 2.5, 1)
    assertEquals(WritePixel(c), "0 255 255")
  }

  test("Write a row") {
    val row = MakeCanvas(Color(0, 0, 0))(3, 3).apply(0)
    val expected = "0 0 0 0 0 0 0 0 0\n"
    assertEquals(WriteRow(row), expected)
  }

  test("Splitting long lines in PPM files") {
    val c = MakeCanvas(Color(1, 0.8, 0.6))(10, 2)
    val ppm = CanvasToPPM(c)
    assertNoDiff(
      ppm.apply(3),
      "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n"
    )
    assertEquals(
      ppm.apply(4),
      "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n"
    )
  }

  test("PPM files are terminated by a newline character") {
    val c = MakeCanvas(5, 3)
    val output = CanvasToPPM(c)
    assert(output(output.length - 1).endsWith("\n"))
  }
}
