// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class ColorSuite extends munit.FunSuite {
  test("Colors are (red, green, blue) tuples") {
    val input = Color(-0.5, 0.4, 1.7)
    assertEquals(input.red, -0.5)
    assertEquals(input.green, 0.4)
    assertEquals(input.blue, 1.7)
  }

  test("Add two colors") {
    val a = Color(0.9, 0.6, 0.75)
    val b = Color(0.7, 0.1, 0.25)
    val expected = Color(1.6, 0.7, 1.0)
    assert(a + b ~= expected)
  }

  test("Subtract two colors") {
    val a = Color(0.9, 0.6, 0.75)
    val b = Color(0.7, 0.1, 0.25)
    val expected = Color(0.2, 0.5, 0.5)
    assert(a - b ~= expected)
  }

  test("Multiplying colors") {
    val a = Color(1, 0.2, 0.4)
    val b = Color(0.9, 1, 0.1)
    val expected = Color(0.9, 0.2, 0.04)
    assert(a * b ~= expected)
  }
}
