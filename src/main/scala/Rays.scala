case class Ray(origin: Tuple, direction: Tuple)

def Position(r: Ray, t: Double): Tuple = {
  r.origin + r.direction * t
}

def Transform(r: Ray, m: IndexedSeq[IndexedSeq[Double]]): Ray = {
  Ray(
    IndexedSeqToTuple(
      MultiplyMatrixTuple(m, TupleToIndexedSeq(r.origin))
    ).get,
    IndexedSeqToTuple(
      MultiplyMatrixTuple(m, TupleToIndexedSeq(r.direction))
    ).get
  )
}
