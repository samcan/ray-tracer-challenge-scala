class LightsSuite extends munit.FunSuite {
  test("A point light has a position and intensity") {
    val intensity = Color(1, 1, 1)
    val position = point(0, 0, 0)
    val light = PointLight(position, intensity)
    assertEquals(light.position, position)
    assertEquals(light.intensity, intensity)
  }
}
