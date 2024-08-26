val EPSILON = 0.00001

case class Tuple(x: Double, y: Double, z: Double, w: Double) {
  def +(b: Tuple): Tuple = {
    Tuple(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w)
  }

  def -(b: Tuple): Tuple = {
    Tuple(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w)
  }

  def *(scalar: Double): Tuple = {
    Tuple(this.x * scalar, this.y * scalar, this.z * scalar, this.w * scalar)
  }

  def /(divisor: Double): Tuple = {
    this * (1.0 / divisor)
  }

  def ~=(b: Tuple): Boolean = {
    math.abs(this.x - b.x) < EPSILON && math.abs(this.y - b.y) < EPSILON && math
      .abs(this.z - b.z) < EPSILON && math.abs(this.w - b.w) < EPSILON
  }
}

/** Determine if the provided tuple is a point.
  *
  * @param t
  *   The tuple to check
  * @return
  *   true if the tuple is a point, false if not.
  */
def tuple_is_a_point(t: Tuple): Boolean = {
  t.w == 1.0
}

/** Determine if the provided tuple is a vector.
  *
  * @param t
  *   The tuple to check.
  * @return
  *   true if the tuple is a vector, false if not.
  */
def tuple_is_a_vector(t: Tuple): Boolean = {
  !tuple_is_a_point(t)
}

/** Create a point tuple.
  *
  * @param x
  *   The x-coord.
  * @param y
  *   The y-coord.
  * @param z
  *   The z-coord (left-handed coordinate system).
  * @return
  *   A tuple that has w = 1.0 (is a point).
  */
def point(x: Double, y: Double, z: Double): Tuple = {
  Tuple(x, y, z, 1.0)
}

/** Create a vector tuple.
  *
  * @param x
  *   The x-coord.
  * @param y
  *   The y-coord.
  * @param z
  *   The z-coord (left-handed coordinate system).
  * @return
  *   A tuple that has w = 0.0 (is a vector).
  */
def vector(x: Double, y: Double, z: Double): Tuple = {
  Tuple(x, y, z, 0.0)
}

// "The smaller the dot product, the larger the angle between the two vectors"
def dot(a: Tuple, b: Tuple): Double = {
  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
}

/** Compute the cross product: the vector that is perpendicular to both vectors
  * a and b.
  *
  * @param a
  *   The first vector.
  * @param b
  *   The second vector.
  * @return
  *   The cross product of the two vectors.
  */
def cross(a: Tuple, b: Tuple): Tuple = {
  vector(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
}

/** Compute the magnitude of the vector.
  *
  * (Can we somehow limit to just a vector type?)
  *
  * @param t
  *   The tuple to operate on.
  * @return
  *   The magnitude of the vector.
  */
def magnitude(t: Tuple): Double = {
  math.sqrt(
    math.pow(t.x, 2) + math.pow(t.y, 2) + math.pow(t.z, 2) + math.pow(t.w, 2)
  )
}

/** Normalize the vector.
  *
  * (Can we somehow limit to just a vector type?)
  *
  * @param t
  *   The tuple to operate on.
  * @return
  *   The normalized vector.
  */
def normalize(t: Tuple): Tuple = {
  val m = magnitude(t)
  Tuple(t.x / m, t.y / m, t.z / m, t.w / m)
}

/** Negate the tuple.
  *
  * @param t
  *   The tuple to negate.
  * @return
  *   The negated tuple.
  */
def negate(t: Tuple): Tuple = {
  Tuple(-t.x, -t.y, -t.z, -t.w)
}

def TupleToIndexedSeq(t: Tuple): IndexedSeq[Double] = {
  Vector[Double](t.x, t.y, t.z, t.w).toIndexedSeq
}

def IndexedSeqToTuple(s: IndexedSeq[Double]): Option[Tuple] = {
  if s.length == 4 then
    Some(Tuple(s.apply(0), s.apply(1), s.apply(2), s.apply(3)))
  else None
}
