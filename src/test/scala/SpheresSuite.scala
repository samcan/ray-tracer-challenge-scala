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

  test("The normal on a sphere at a point on the x axis") {
    val s = Sphere()
    assertEquals(Normal(s, point(1, 0, 0)).get, vector(1, 0, 0))
  }

  test("The normal on a sphere at a point on the y axis") {
    val s = Sphere()
    assertEquals(Normal(s, point(0, 1, 0)).get, vector(0, 1, 0))
  }

  test("The normal on a sphere at a point on the z axis") {
    val s = Sphere()
    assertEquals(Normal(s, point(0, 0, 1)).get, vector(0, 0, 1))
  }

  test("The normal on a sphere at a nonaxial point") {
    val s = Sphere()
    assertEquals(
      Normal(
        s,
        point(math.sqrt(3) / 3, math.sqrt(3) / 3, math.sqrt(3) / 3)
      ).get,
      vector(math.sqrt(3) / 3, math.sqrt(3) / 3, math.sqrt(3) / 3)
    )
  }

  test("The normal is a normalized vector") {
    val s = Sphere()
    val n =
      Normal(s, point(math.sqrt(3) / 3, math.sqrt(3) / 3, math.sqrt(3) / 3))
    assertEquals(n.get, normalize(n.get))
  }

  test("Computing the normal on a translated sphere") {
    val s = Sphere()
    val s2 = s.copy(transform = Translation(0, 1, 0))
    assert(
      Normal(s2, point(0, 1.70711, -0.70711)).get ~= vector(
        0,
        0.70711,
        -0.70711
      )
    )
  }

  test("Computing the normal on a transformed sphere") {
    val s = Sphere()
    val s2 = s.copy(transform =
      MultiplyMatrix(Scaling(1, 0.5, 1), RotationZ(math.Pi / 5))
    )
    assert(
      Normal(s2, point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2)).get ~= vector(
        0,
        0.97014,
        -0.24254
      )
    )
  }

  test("A sphere has a default material") {
    val s = Sphere()
    assertEquals(s.material, Material())
  }

  test("A sphere may be assigned a material") {
    val s = Sphere()
    val m = Material().copy(ambient = 1)
    val s2 = s.copy(material = m)
    assertEquals(s2.material, m)
  }
}
