// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class TupleSuite extends munit.FunSuite {
  test("A tuple with w=1.0 is a point") {
    val input = Tuple(4.3, -4.2, 3.1, 1.0)
    assertEquals(tuple_is_a_point(input), true)
    assertEquals(tuple_is_a_vector(input), false)
  }

  test("A tuple with w=0.0 is a vector") {
    val input = Tuple(4.3, -4.2, 3.1, 0.0)
    assertEquals(tuple_is_a_vector(input), true)
    assertEquals(tuple_is_a_point(input), false)
  }

  test("point() creates a tuple with w=1.0") {
    val output = point(4, -4, 3)
    assertEquals(tuple_is_a_point(output), true)
  }

  test("vector() creates a tuple with w=0.0") {
    val output = vector(4, -4, 3)
    assertEquals(tuple_is_a_vector(output), true)
  }
}
