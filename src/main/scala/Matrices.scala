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
