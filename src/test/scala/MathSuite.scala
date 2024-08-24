class MathSuite extends munit.FunSuite {
  test("Test value within range isn't clamped") {
    val min = -5
    val max = 5
    val input = 3

    val clamped = Clamp(min, max)(input)
    assertEquals(clamped, input)
  }

  test("Test value at min of range isn't clamped") {
    val min = -5
    val max = 5
    val input = min

    val clamped = Clamp(min, max)(input)
    assertEquals(clamped, input)
  }

  test("Test value at max of range isn't clamped") {
    val min = -5
    val max = 5
    val input = max

    val clamped = Clamp(min, max)(input)
    assertEquals(clamped, input)
  }

  test("Test value less than range is clamped") {
    val min = -5
    val max = 5
    val input = min - 1

    val clamped = Clamp(min, max)(input)
    assertEquals(clamped, min)
  }

  test("Test value greater than range is clamped") {
    val min = -5
    val max = 5
    val input = max + 1

    val clamped = Clamp(min, max)(input)
    assertEquals(clamped, max)
  }
}
