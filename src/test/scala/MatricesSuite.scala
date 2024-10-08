import org.scalactic._
import org.scalactic.TripleEquals._

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
    val product = IndexedSeqToTuple(MultiplyMatrixTuple(a, b))
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

  test("Calculating the inverse of a matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](-5, 2, 6, -8),
      IndexedSeq[Double](1, -5, 1, 8),
      IndexedSeq[Double](7, 7, -6, -7),
      IndexedSeq[Double](1, -3, 7, 4)
    )
    val b = Inverse(Some(a))
    val expectedInverse = IndexedSeq(
      IndexedSeq[Double](0.21805, 0.45113, 0.24060, -0.04511),
      IndexedSeq[Double](-0.80827, -1.45677, -0.44361, 0.52068),
      IndexedSeq[Double](-0.07895, -0.22368, -0.05263, 0.19737),
      IndexedSeq[Double](-0.52256, -0.81391, -0.30075, 0.30639)
    )

    assertEquals(Determinant(Some(a)), Some(532.0))
    assertEquals(Cofactor(a, 2, 3), Some(-160.0))
    assertEquals(GetMatrixValue(b.get, 3, 2), Some(-160.0 / 532.0))
    assertEquals(Cofactor(a, 3, 2), Some(105.0))
    assertEquals(GetMatrixValue(b.get, 2, 3), Some(105.0 / 532.0))
    assert(b.get === expectedInverse)
  }

  test("Calculating the inverse of another matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](8, -5, 9, 2),
      IndexedSeq[Double](7, 5, 6, 1),
      IndexedSeq[Double](-6, 0, 9, 6),
      IndexedSeq[Double](-3, 0, -9, -4)
    )
    val expectedInverse = IndexedSeq(
      IndexedSeq[Double](-0.15385, -0.15385, -0.28205, -0.53846),
      IndexedSeq[Double](-0.07692, 0.12308, 0.02564, 0.03077),
      IndexedSeq[Double](0.35897, 0.35897, 0.43590, 0.92308),
      IndexedSeq[Double](-0.69231, -0.69231, -0.76923, -1.92308)
    )

    assert(Inverse(Some(a)).get === expectedInverse)
  }

  test("Calculating the inverse of a third matrix") {
    val a = IndexedSeq(
      IndexedSeq[Double](9, 3, 0, 9),
      IndexedSeq[Double](-5, -2, -6, -3),
      IndexedSeq[Double](-4, 9, 6, 4),
      IndexedSeq[Double](-7, 6, 6, 2)
    )
    val expectedInverse = IndexedSeq(
      IndexedSeq[Double](-0.04074, -0.07778, 0.14444, -0.22222),
      IndexedSeq[Double](-0.07778, 0.03333, 0.36667, -0.33333),
      IndexedSeq[Double](-0.02901, -0.14630, -0.10926, 0.12963),
      IndexedSeq[Double](0.17778, 0.06667, -0.26667, 0.33333)
    )

    assert(Inverse(Some(a)).get === expectedInverse)
  }

  test("Multiplying a product by its inverse") {
    val a = IndexedSeq(
      IndexedSeq[Double](3, -9, 7, 3),
      IndexedSeq[Double](3, -8, 2, -9),
      IndexedSeq[Double](-4, 4, 4, 1),
      IndexedSeq[Double](-6, 5, -1, 1)
    )
    val b = IndexedSeq(
      IndexedSeq[Double](8, 2, 2, 2),
      IndexedSeq[Double](3, -1, 7, 0),
      IndexedSeq[Double](7, 0, 5, 4),
      IndexedSeq[Double](6, -2, 0, 5)
    )
    val c = MultiplyMatrix(a, b)
    assert(MultiplyMatrix(c, Inverse(Some(b)).get) === a)
  }

  test("Multiplying by a translation matrix") {
    val transform = Translation(5.0, -3.0, 2.0)
    val p = TupleToIndexedSeq(point(-3, 4, 5))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(point(2, 1, 7))
    )
  }

  test("Multiplying by the inverse of a translation matrix") {
    val transform = Inverse(Some(Translation(5.0, -3.0, 2.0))).get
    val p = TupleToIndexedSeq(point(-3, 4, 5))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(point(-8, 7, 3))
    )
  }

  test("Translation does not affect vectors") {
    val transform = Translation(5.0, -3.0, 2.0)
    val p = TupleToIndexedSeq(vector(-3, 4, 5))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(vector(-3, 4, 5))
    )
  }

  test("A scaling matrix applied to a point") {
    val transform = Scaling(2.0, 3.0, 4.0)
    val p = TupleToIndexedSeq(point(-4, 6, 8))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(point(-8, 18, 32))
    )
  }

  test("A scaling matrix applied to a vector") {
    val transform = Scaling(2.0, 3.0, 4.0)
    val p = TupleToIndexedSeq(vector(-4, 6, 8))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        vector(-8, 18, 32)
      )
    )
  }

  test("Multiplying by the inverse of a scaling matrix") {
    val transform = Inverse(Some(Scaling(2.0, 3.0, 4.0))).get
    val p = TupleToIndexedSeq(vector(-4, 6, 8))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(vector(-2, 2, 2))
    )
  }

  test("Reflection is scaling by a negative value") {
    val transform = Scaling(-1.0, 1.0, 1.0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(point(-2, 3, 4))
    )
  }

  test("Rotating a point around the x axis") {
    val p = TupleToIndexedSeq(point(0, 1, 0))
    val half_quarter = RotationX(math.Pi / 4)
    val full_quarter = RotationX(math.Pi / 2)

    assert(
      MultiplyMatrixTuple(half_quarter, p) === TupleToIndexedSeq(
        point(0, math.sqrt(2) / 2, math.sqrt(2) / 2)
      )
    )
    assert(
      MultiplyMatrixTuple(full_quarter, p) === TupleToIndexedSeq(
        point(0, 0, 1)
      )
    )
  }

  test("The inverse of an x-rotation rotates in the opposite direction") {
    val p = TupleToIndexedSeq(point(0, 1, 0))
    val half_quarter = Inverse(Some(RotationX(math.Pi / 4))).get

    assert(
      MultiplyMatrixTuple(half_quarter, p) === TupleToIndexedSeq(
        point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2)
      )
    )
  }

  test("Rotating a point around the y axis") {
    val p = TupleToIndexedSeq(point(0, 0, 1))
    val half_quarter = RotationY(math.Pi / 4)
    val full_quarter = RotationY(math.Pi / 2)

    assert(
      MultiplyMatrixTuple(half_quarter, p) === TupleToIndexedSeq(
        point(math.sqrt(2) / 2, 0, math.sqrt(2) / 2)
      )
    )
    assert(
      MultiplyMatrixTuple(full_quarter, p) === TupleToIndexedSeq(
        point(1, 0, 0)
      )
    )
  }

  test("Rotating a point around the z axis") {
    val p = TupleToIndexedSeq(point(0, 1, 0))
    val half_quarter = RotationZ(math.Pi / 4)
    val full_quarter = RotationZ(math.Pi / 2)

    assert(
      MultiplyMatrixTuple(half_quarter, p) === TupleToIndexedSeq(
        point(-math.sqrt(2) / 2, math.sqrt(2) / 2, 0)
      )
    )
    assert(
      MultiplyMatrixTuple(full_quarter, p) === TupleToIndexedSeq(
        point(-1, 0, 0)
      )
    )
  }

  test("A shearing transformation moves x in proportion to y") {
    val transform = Shearing(1, 0, 0, 0, 0, 0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(5, 3, 4)
      )
    )
  }

  test("A shearing transformation moves x in proportion to z") {
    val transform = Shearing(0, 1, 0, 0, 0, 0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(6, 3, 4)
      )
    )
  }

  test("A shearing transformation moves y in proportion to x") {
    val transform = Shearing(0, 0, 1, 0, 0, 0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(2, 5, 4)
      )
    )
  }

  test("A shearing transformation moves y in proportion to z") {
    val transform = Shearing(0, 0, 0, 1, 0, 0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(2, 7, 4)
      )
    )
  }

  test("A shearing transformation moves z in proportion to x") {
    val transform = Shearing(0, 0, 0, 0, 1, 0)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(2, 3, 6)
      )
    )
  }

  test("A shearing transformation moves z in proportion to y") {
    val transform = Shearing(0, 0, 0, 0, 0, 1)
    val p = TupleToIndexedSeq(point(2, 3, 4))
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(
        point(2, 3, 7)
      )
    )
  }

  test("Individual transformations are applied in sequence") {
    val p = TupleToIndexedSeq(point(1, 0, 1))
    val transformA = RotationX(math.Pi / 2)
    val transformB = Scaling(5, 5, 5)
    val transformC = Translation(10, 5, 7)

    // apply rotation first
    val p2 = MultiplyMatrixTuple(transformA, p)
    assert(p2 === TupleToIndexedSeq(point(1, -1, 0)))

    // then apply scaling
    val p3 = MultiplyMatrixTuple(transformB, p2)
    assert(p3 === TupleToIndexedSeq(point(5, -5, 0)))

    // then apply transformation
    val p4 = MultiplyMatrixTuple(transformC, p3)
    assert(p4 === TupleToIndexedSeq(point(15, 0, 7)))
  }

  test("Chained transformations must be applied in reverse order") {
    val p = TupleToIndexedSeq(point(1, 0, 1))
    val transformA = RotationX(math.Pi / 2)
    val transformB = Scaling(5, 5, 5)
    val transformC = Translation(10, 5, 7)

    val transform = MultiplyMatrix(MultiplyMatrix(transformC, transformB), transformA)
    assert(
      MultiplyMatrixTuple(transform, p) === TupleToIndexedSeq(point(15, 0, 7))
    )
  }
}
