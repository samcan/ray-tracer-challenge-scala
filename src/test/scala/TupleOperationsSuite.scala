// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class TupleOperationsSuite extends munit.FunSuite {
  test("Add two tuples") {
    val a = Tuple(3, -2, 5, 1)
    val b = Tuple(-2, 3, 1, 0)
    val expected = Tuple(1, 1, 6, 1)
    assertEquals(a + b, expected)
  }

  test("Subtract two points") {
    val a = point(3, 2, 1)
    val b = point(5, 6, 7)
    val expected = vector(-2, -4, -6)
    assertEquals(a - b, expected)
  }

  test("Subtract a vector from a point") {
    val a = point(3, 2, 1)
    val b = vector(5, 6, 7)
    val expected = point(-2, -4, -6)
    assertEquals(a - b, expected)
  }

  test("Subtract two vectors") {
    val a = vector(3, 2, 1)
    val b = vector(5, 6, 7)
    val expected = vector(-2, -4, -6)
    assertEquals(a - b, expected)
  }

  test("Negate a tuple") {
    val a = Tuple(1, -2, 3, -4)
    val expected = Tuple(-1, 2, -3, 4)
    assertEquals(a.negate(), expected)
  }

  test("Multiplying a tuple by a scalar") {
    val a = Tuple(1, -2, 3, -4)
    val expected = Tuple(3.5, -7, 10.5, -14)
    assertEquals(a * 3.5, expected)
  }

  test("Multiplying a tuple by a fraction") {
    val a = Tuple(1, -2, 3, -4)
    val expected = Tuple(0.5, -1, 1.5, -2)
    assertEquals(a * 0.5, expected)
  }

  test("Dividing a tuple by a scalar") {
    val a = Tuple(1, -2, 3, -4)
    val expected = Tuple(0.5, -1, 1.5, -2)
    assertEquals(a / 2, expected)
  }

  test("Compute the magnitude of vector(1, 0, 0)") {
    val a = vector(1, 0, 0)
    assertEquals(a.magnitude(), 1.0)
  }

  test("Compute the magnitude of vector(0, 1, 0)") {
    val a = vector(0, 1, 0)
    assertEquals(a.magnitude(), 1.0)
  }

  test("Compute the magnitude of vector(0, 0, 1)") {
    val a = vector(0, 0, 1)
    assertEquals(a.magnitude(), 1.0)
  }

  test("Compute the magnitude of vector(1, 2, 3)") {
    val a = vector(1, 2, 3)
    assertEquals(a.magnitude(), Math.sqrt(14))
  }

  test("Compute the magnitude of vector(-1, -2, -3)") {
    val a = vector(-1, -2, -3)
    assertEquals(a.magnitude(), Math.sqrt(14))
  }

  test("Normalizing vector(4, 0, 0) gives (1, 0, 0)") {
    val a = vector(4, 0, 0)
    val expected = vector(1, 0, 0)
    assertEquals(a.normalize(), expected)
  }

  test("Normalizing vector(1, 2, 3)") {
    val a = vector(1, 2, 3)
    val expected =
      vector(1 / Math.sqrt(14), 2 / Math.sqrt(14), 3 / Math.sqrt(14))
    assertEquals(a.normalize(), expected)
  }

  test("The dot product of two tuples") {
    val a = vector(1, 2, 3)
    val b = vector(2, 3, 4)
    assertEquals(dot(a, b), 20.0)
  }

  test("The cross product of two vectors") {
    val a = vector(1, 2, 3)
    val b = vector(2, 3, 4)
    assertEquals(cross(a, b), vector(-1, 2, -1))
    assertEquals(cross(b, a), vector(1, -2, 1))
  }
}
