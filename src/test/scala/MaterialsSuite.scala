class MaterialsSuite extends munit.FunSuite {
  test("The default material") {
    val m = Material()
    assertEquals(m.color, Color(1, 1, 1))
    assertEquals(m.ambient, 0.1)
    assertEquals(m.diffuse, 0.9)
    assertEquals(m.specular, 0.9)
    assertEquals(m.shininess, 200.0)
  }

  test("Lighting with the eye between the light and the surface") {
    val mat = Material()
    val position = point(0, 0, 0)

    val eye = vector(0, 0, -1)
    val norm = vector(0, 0, -1)
    val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
    assertEquals(
      Lighting(mat, light, position, eye, norm),
      Color(1.9, 1.9, 1.9)
    )
  }

  test(
    "Lighting with the eye between the light and the surface, eye offset 45°"
  ) {
    val mat = Material()
    val position = point(0, 0, 0)

    val eye = vector(0, math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val norm = vector(0, 0, -1)
    val light = PointLight(point(0, 0, -10), Color(1, 1, 1))
    assertEquals(
      Lighting(mat, light, position, eye, norm),
      Color(1.0, 1.0, 1.0)
    )
  }

  test(
    "Lighting with the eye opposite surface, light offset 45°"
  ) {
    val mat = Material()
    val position = point(0, 0, 0)

    val eye = vector(0, 0, -1)
    val norm = vector(0, 0, -1)
    val light = PointLight(point(0, 10, -10), Color(1, 1, 1))
    assert(
      Lighting(mat, light, position, eye, norm) ~=
        Color(0.7364, 0.7364, 0.7364)
    )
  }

  test(
    "Lighting with the eye in the path of the reflection vector"
  ) {
    val mat = Material()
    val position = point(0, 0, 0)

    val eye = vector(0, -math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val norm = vector(0, 0, -1)
    val light = PointLight(point(0, 10, -10), Color(1, 1, 1))
    assert(
      Lighting(mat, light, position, eye, norm) ~=
        Color(1.6364, 1.6364, 1.6364)
    )
  }

  test(
    "Lighting with the light behind the surface"
  ) {
    val mat = Material()
    val position = point(0, 0, 0)

    val eye = vector(0, 0, -1)
    val norm = vector(0, 0, -1)
    val light = PointLight(point(0, 0, 10), Color(1, 1, 1))
    assert(
      Lighting(mat, light, position, eye, norm) ~=
        Color(0.1, 0.1, 0.1)
    )
  }
}
