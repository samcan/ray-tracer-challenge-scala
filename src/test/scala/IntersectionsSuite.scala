class IntersectionsSuite extends munit.FunSuite {
  test("An intersection encapsulates t and object") {
    val s = Sphere()
    val i = Intersection(3.5, s)

    assertEquals(i.t, 3.5)
    assertEquals(i.obj, s)
  }

  test("Aggregating intersections") {
    val s = Sphere()
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = List[Intersection](i1, i2)

    assertEquals(xs.size, 2)
    assertEquals(xs, List[Intersection](i1, i2))
  }

  test("The hit, when all intersections have positive t") {
    val s = Sphere()
    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val xs = List[Intersection](i2, i1)

    assertEquals(Hit(xs), Some(i1))
  }

  test("The hit, when some intersections have negative t") {
    val s = Sphere()
    val i1 = Intersection(-1, s)
    val i2 = Intersection(1, s)
    val xs = List[Intersection](i2, i1)

    assertEquals(Hit(xs), Some(i2))
  }

  test("The hit, when all intersections have negative t") {
    val s = Sphere()
    val i1 = Intersection(-2, s)
    val i2 = Intersection(-1, s)
    val xs = List[Intersection](i2, i1)

    assertEquals(Hit(xs), None)
  }

  test("The hit is always the lowest nonnegative intersection") {
    val s = Sphere()
    val i1 = Intersection(5, s)
    val i2 = Intersection(7, s)
    val i3 = Intersection(-3, s)
    val i4 = Intersection(2, s)
    val xs = List[Intersection](i1, i2, i3, i4)

    assertEquals(Hit(xs), Some(i4))
  }
}
