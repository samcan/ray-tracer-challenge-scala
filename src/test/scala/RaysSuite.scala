class RaysSuite extends munit.FunSuite {
  test("Creating and querying a ray") {
    val origin = point(1, 2, 3)
    val direction = vector(4, 5, 6)
    val r = Ray(origin, direction)

    assertEquals(r.origin, origin)
    assertEquals(r.direction, direction)
  }

  test("Computing a point from a distance") {
    val r = Ray(point(2, 3, 4), vector(1, 0, 0))

    assertEquals(Position(r, 0), point(2, 3, 4))
    assertEquals(Position(r, 1), point(3, 3, 4))
    assertEquals(Position(r, -1), point(1, 3, 4))
    assertEquals(Position(r, 2.5), point(4.5, 3, 4))
  }

  test("Translating a ray") {
    val r = Ray(point(1, 2, 3), vector(0, 1, 0))
    val m = Translation(3, 4, 5)

    val r2 = Transform(r, m)
    assertEquals(r2.origin, point(4, 6, 8))
    assertEquals(r2.direction, vector(0, 1, 0))
  }

  test("Scaling a ray") {
    val r = Ray(point(1, 2, 3), vector(0, 1, 0))
    val m = Scaling(2, 3, 4)

    val r2 = Transform(r, m)
    assertEquals(r2.origin, point(2, 6, 12))
    assertEquals(r2.direction, vector(0, 3, 0))
  }
}
