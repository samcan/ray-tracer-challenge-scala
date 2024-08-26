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

  test("Multiply 4x4 matrix with identity matrix") {
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

  test("Calculate determinant of 2x2 matrix") {
    val a = IndexedSeq(IndexedSeq[Double](1, 5), IndexedSeq[Double](-3, 2))
    assertEquals(Determinant(Some(a)), Some(17.0))
  }

  test("Remove the beginning col from a row") {
    val row = IndexedSeq[Double](1, 2, 3)
    assertEquals(FilterMatrixColFromRow(0)(row), Some(IndexedSeq[Double](2, 3)))
  }

  test("Remove the ending col from a row") {
    val row = IndexedSeq[Double](1, 2, 3)
    assertEquals(FilterMatrixColFromRow(2)(row), Some(IndexedSeq[Double](1, 2)))
  }

  test("Remove an inner col from a row") {
    val row = IndexedSeq[Double](1, 2, 3)
    assertEquals(FilterMatrixColFromRow(1)(row), Some(IndexedSeq[Double](1, 3)))
  }

  test("Remove an out-of-range col from a row fails") {
    val row = IndexedSeq[Double](1, 2, 3)
    assertEquals(FilterMatrixColFromRow(5)(row), None)
  }

  test("A submatrix of a 3x3 matrix is a 2x2 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](1, 5, 0),
      IndexedSeq[Double](-3, 2, 7),
      IndexedSeq[Double](0, 6, -3)
    )
    val expected =
      IndexedSeq(IndexedSeq[Double](-3, 2), IndexedSeq[Double](0, 6))
    assertEquals(Submatrix(a, 0, 2), Some(expected))
  }

  test("A submatrix of a 4x4 matrix is a 3x3 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](-6, 1, 1, 6),
      IndexedSeq[Double](-8, 5, 8, 6),
      IndexedSeq[Double](-1, 0, 8, 2),
      IndexedSeq[Double](-7, 1, -1, 1)
    )
    val expected = IndexedSeq(
      IndexedSeq[Double](-6, 1, 6),
      IndexedSeq[Double](-8, 8, 6),
      IndexedSeq[Double](-7, -1, 1)
    )
    assertEquals(Submatrix(a, 2, 1), Some(expected))
  }

  test("Calculating a minor of a 3x3 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](3, 5, 0),
      IndexedSeq[Double](2, -1, -7),
      IndexedSeq[Double](6, -1, 5)
    )
    assertEquals(Minor(a, 1, 0), Some(25.0))
  }

  test("Calculating a cofactor of a 3x3 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](3, 5, 0),
      IndexedSeq[Double](2, -1, -7),
      IndexedSeq[Double](6, -1, 5)
    )
    assertEquals(Cofactor(a, 0, 0), Some(-12.0))
    assertEquals(Cofactor(a, 1, 0), Some(-25.0))
  }

  test("Calculating the determinant of a 3x3 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](1, 2, 6),
      IndexedSeq[Double](-5, 8, -4),
      IndexedSeq[Double](2, 6, 4)
    )
    assertEquals(Cofactor(a, 0, 0), Some(56.0))
    assertEquals(Cofactor(a, 0, 1), Some(12.0))
    assertEquals(Cofactor(a, 0, 2), Some(-46.0))
    assertEquals(Determinant(Some(a)), Some(-196.0))
  }

  test("Calculating the determinant of a 4x4 matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](-2, -8, 3, 5),
      IndexedSeq[Double](-3, 1, 7, 3),
      IndexedSeq[Double](1, 2, -9, 6),
      IndexedSeq[Double](-6, 7, 7, -9)
    )
    assertEquals(Cofactor(a, 0, 0), Some(690.0))
    assertEquals(Cofactor(a, 0, 1), Some(447.0))
    assertEquals(Cofactor(a, 0, 2), Some(210.0))
    assertEquals(Cofactor(a, 0, 3), Some(51.0))
    assertEquals(Determinant(Some(a)), Some(-4071.0))
  }

  test("Testing an invertible matrix for invertibility") {
    val a = IndexedSeq(
      IndexedSeq[Double](6, 4, 4, 4),
      IndexedSeq[Double](5, 5, 7, 6),
      IndexedSeq[Double](4, -9, 3, 7),
      IndexedSeq[Double](9, 1, 7, -6)
    )
    assertEquals(Invertible(Some(a)), Some(true))
  }

  test("Testing a noninvertible matrix for invertibility") {
    val a = IndexedSeq(
      IndexedSeq[Double](-4, 2, -2, -3),
      IndexedSeq[Double](9, 6, 2, 6),
      IndexedSeq[Double](0, -5, 1, -5),
      IndexedSeq[Double](0, 0, 0, 0)
    )
    assertEquals(Invertible(Some(a)), Some(false))
  }
}
