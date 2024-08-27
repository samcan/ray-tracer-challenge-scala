class SpheresSuite extends munit.FunSuite {
  test("A ray intersects a sphere at two points") {
    val r = Ray(point(0, 0, -5), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 2)
    assertEquals(
      xs,
      List[Intersection](Intersection(4.0, s.id), Intersection(6.0, s.id))
    )
  }

  test("A ray intersects a sphere at a tangent") {
    val r = Ray(point(0, 1, -5), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 2)
    assertEquals(
      xs,
      List[Intersection](Intersection(5.0, s.id), Intersection(5.0, s.id))
    )
  }

  test("A ray misses a sphere") {
    val r = Ray(point(0, 2, -5), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 0)
  }

  test("A ray originates inside a sphere") {
    val r = Ray(point(0, 0, 0), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 2)
    assertEquals(
      xs,
      List[Intersection](Intersection(-1.0, s.id), Intersection(1.0, s.id))
    )
  }

  test("A sphere is behind a ray") {
    val r = Ray(point(0, 0, 5), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 2)
    assertEquals(
      xs,
      List[Intersection](Intersection(-6.0, s.id), Intersection(-4.0, s.id))
    )
  }

  test("Intersect sets the object on the intersection") {
    val r = Ray(point(0, 0, -5), vector(0, 0, 1))
    val s = Sphere()

    val xs = Intersect(s, r)

    assertEquals(xs.size, 2)
    assertEquals(xs.map(i => i.objectId), List[java.util.UUID](s.id, s.id))
  }

  test("A sphere's default transformation") {
    val s = Sphere()
    assertEquals(s.transform, IdentityMatrix())
  }

  test("Changing a sphere's transformation") {
    val s = Sphere()
    val t = Translation(2, 3, 4)

    val s2 = s.copy(transform = t)
    assertEquals(s2.transform, t)
  }

  test("Intersecting a scaled sphere with a ray") {
    val r = Ray(point(0, 0, -5), vector(0, 0, 1))
    val s = Sphere()
    val s2 = s.copy(transform = Scaling(2, 2, 2))

    val xs = Intersect(s2, r)

    assertEquals(xs.size, 2)
    assertEquals(xs.map(i => i.t), List[Double](3, 7))
  }

  test("Intersecting a translated sphere with a ray") {
    val r = Ray(point(0, 0, -5), vector(0, 0, 1))
    val s = Sphere()
    val s2 = s.copy(transform = Translation(5, 0, 0))

    val xs = Intersect(s2, r)

    assertEquals(xs.size, 0)
  }
}
