class MatricesSuite extends munit.FunSuite {
  test("Inspecting a 4x4 matrix") {
    val m = Vector(
      Vector[Double](1, 2, 3, 4),
      Vector[Double](5.5, 6.5, 7.5, 8.5),
      Vector[Double](9, 10, 11, 12),
      Vector[Double](13.5, 14.5, 15.5, 16.5)
    )

    assertEquals(GetMatrixValue(m, 0, 0), Some(1.0))
    assertEquals(GetMatrixValue(m, 0, 3), Some(4.0))
    assertEquals(GetMatrixValue(m, 1, 0), Some(5.5))
    assertEquals(GetMatrixValue(m, 1, 2), Some(7.5))
    assertEquals(GetMatrixValue(m, 2, 2), Some(11.0))
    assertEquals(GetMatrixValue(m, 3, 0), Some(13.5))
    assertEquals(GetMatrixValue(m, 3, 2), Some(15.5))
  }

  test("Inspecting a 2x2 matrix") {
    val m = Vector(
      Vector[Double](-3, 5),
      Vector[Double](1, -2)
    )

    assertEquals(GetMatrixValue(m, 0, 0), Some(-3.0))
    assertEquals(GetMatrixValue(m, 0, 1), Some(5.0))
    assertEquals(GetMatrixValue(m, 1, 0), Some(1.0))
    assertEquals(GetMatrixValue(m, 1, 1), Some(-2.0))
  }

  test("Inspecting a 3x3 matrix") {
    val m = Vector(
      Vector[Double](-3, 5, 0),
      Vector[Double](1, -2, -7),
      Vector[Double](0, 1, 1)
    )

    assertEquals(GetMatrixValue(m, 0, 0), Some(-3.0))
    assertEquals(GetMatrixValue(m, 1, 1), Some(-2.0))
    assertEquals(GetMatrixValue(m, 2, 2), Some(1.0))
  }

  test("Testing 3x3 matrix equals identical matrix") {
    val a = Vector(
      Vector[Double](-3, 5, 0),
      Vector[Double](1, -2, -7),
      Vector[Double](0, 1, 1)
    )
    val b = Vector(
      Vector[Double](-3, 5, 0),
      Vector[Double](1, -2, -7),
      Vector[Double](0, 1, 1)
    )

    assertEquals(a, b)
  }

  test("Testing 3x3 matrix doesn't equal different matrix") {
    val a = Vector(
      Vector[Double](-3, 5, 0),
      Vector[Double](1, -2, -7),
      Vector[Double](0, 1, 1)
    )
    val b = Vector(
      Vector[Double](3, 5, 0),
      Vector[Double](1, 2, -7),
      Vector[Double](0, 1, -1)
    )

    assertNotEquals(a, b)
  }

  test("Multiply 4x4 matrices") {
    val a = Vector(
      Vector[Double](1, 2, 3, 4),
      Vector[Double](5, 6, 7, 8),
      Vector[Double](9, 8, 7, 6),
      Vector[Double](5, 4, 3, 2)
    )
    val b = Vector(
      Vector[Double](-2, 1, 2, 3),
      Vector[Double](3, 2, 1, -1),
      Vector[Double](4, 3, 6, 5),
      Vector[Double](1, 2, 7, 8)
    )
    val expected = Vector(
      Vector[Double](20, 22, 50, 48),
      Vector[Double](44, 54, 114, 108),
      Vector[Double](40, 58, 110, 102),
      Vector[Double](16, 26, 46, 42)
    )
    val product = MultiplyMatrix(a, b)
    assertEquals(product, expected)
  }

  test("Multiply 4x4 matrix with tuple") {
    val a = Vector(
      Vector[Double](1, 2, 3, 4),
      Vector[Double](2, 4, 4, 2),
      Vector[Double](8, 6, 4, 1),
      Vector[Double](0, 0, 0, 1)
    )
    val b = TupleToIndexedSeq(Tuple(1, 2, 3, 1))
    val product = IndexedSeqToTuple(MultiplyMatrixVector(a, b))
    assertEquals(product, Some(Tuple(18, 24, 33, 1)))
  }

  test("Mutiply 4x4 matrix with identity matrix") {
    val a = Vector(
      Vector[Double](0, 1, 2, 4),
      Vector[Double](1, 2, 4, 8),
      Vector[Double](2, 4, 8, 16),
      Vector[Double](4, 8, 16, 32)
    )
    assertEquals(MultiplyMatrix(a, IdentityMatrix()), a)
  }

  test("Transpose a 4x4 matrix") {
    val a = Vector(
      Vector[Double](0, 9, 3, 0),
      Vector[Double](9, 8, 0, 8),
      Vector[Double](1, 8, 5, 3),
      Vector[Double](0, 0, 5, 8)
    )
    val expected = Vector(
      Vector[Double](0, 9, 1, 0),
      Vector[Double](9, 8, 8, 0),
      Vector[Double](3, 0, 5, 5),
      Vector[Double](0, 8, 3, 8)
    )
    assertEquals(a.transpose, expected)
  }

  test("Transpose the identity matrix gives the identity matrix") {
    val a = IdentityMatrix()
    assertEquals(a.transpose, IdentityMatrix())
  }
}
