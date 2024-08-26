def GetMatrixValue(
    matrix: IndexedSeq[IndexedSeq[Double]],
    y: Int,
    x: Int
): Option[Double] = {
  if y < matrix.length && x < matrix(y).length then Some(matrix(y)(x))
  else None
}

// see https://medium.com/@eob/how-you-might-create-a-scala-matrix-library-in-a-functional-programming-style-760f8bf6ee6
// see ChatGPT
def DotProduct(
    a: IndexedSeq[Double],
    b: IndexedSeq[Double]
): Double = {
  a.zip(b).map { case (x, y) => x * y }.sum
}

// see ChatGPT
def MultiplyMatrix(
    a: IndexedSeq[IndexedSeq[Double]],
    b: IndexedSeq[IndexedSeq[Double]]
): IndexedSeq[IndexedSeq[Double]] = {
  for (aRow <- a) yield for (bCol <- b.transpose) yield DotProduct(aRow, bCol)
}

// https://stackoverflow.com/q/60591619
def MultiplyMatrixVector(
    a: IndexedSeq[IndexedSeq[Double]],
    b: IndexedSeq[Double]
): IndexedSeq[Double] = {
  (a, b).zipped.map((x, y) => (x, b).zipped.map(_ * _).sum)
}

def IdentityMatrix(): IndexedSeq[IndexedSeq[Double]] = {
  IndexedSeq(
    IndexedSeq[Double](1, 0, 0, 0),
    IndexedSeq[Double](0, 1, 0, 0),
    IndexedSeq[Double](0, 0, 1, 0),
    IndexedSeq[Double](0, 0, 0, 1)
  )
}

def Determinant(a: Option[IndexedSeq[IndexedSeq[Double]]]): Option[Double] = {
  a match {
    case Some(m) =>
      if m.length == 2 && m.apply(0).length == 2 then
        Some(
          m(0)
            .zip(m(1).reverse)
            .map { case (x, y) => x * y }
            .reduceLeft((a, b) => a - b)
        )
      else
        Some(
          m(0).zipWithIndex
            .map((n, i) => n * Cofactor(m, 0, i).getOrElse(0.0))
            .sum
        )
    case None => None
  }
}

def FilterMatrixColFromRow(x: Int)(
    row: IndexedSeq[Double]
): Option[IndexedSeq[Double]] = {
  if x < row.length then Some(row.patch(x, IndexedSeq[Double](), 1))
  else None
}

def Submatrix(
    a: IndexedSeq[IndexedSeq[Double]],
    y: Int,
    x: Int
): Option[IndexedSeq[IndexedSeq[Double]]] = {
  // first remove the col from each row, and then remove the row
  Some(a.flatMap(FilterMatrixColFromRow(x)).patch(y, IndexedSeq(), 1))
}

def Minor(a: IndexedSeq[IndexedSeq[Double]], y: Int, x: Int): Option[Double] = {
  Determinant(Submatrix(a, y, x))
}

def Cofactor(
    a: IndexedSeq[IndexedSeq[Double]],
    y: Int,
    x: Int
): Option[Double] = {
  Minor(a, y, x).map(num => if (x + y) % 2 == 0 then num else -num)
}

def Invertible(a: Option[IndexedSeq[IndexedSeq[Double]]]): Option[Boolean] = {
  Determinant(a) match {
    case Some(d) => if d != 0 then Some(true) else Some(false)
    case None    => None
  }
}
