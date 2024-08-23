@main def hello(): Unit =
  println("Hello world!")

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

  def negate(): Tuple = {
    Tuple(-this.x, -this.y, -this.z, -this.w)
  }

  // can we somehow limit the magnitude and normalization to just vectors?
  def magnitude(): Double = {
    Math.sqrt(
      Math.pow(this.x, 2) + Math.pow(this.y, 2) + Math.pow(this.z, 2) + Math
        .pow(this.w, 2)
    )
  }

  def normalize(): Tuple = {
    val magnitude = this.magnitude()
    Tuple(
      this.x / magnitude,
      this.y / magnitude,
      this.z / magnitude,
      this.w / magnitude
    )
  }
}

def tuple_is_a_point(t: Tuple): Boolean = {
  t.w == 1.0
}

def tuple_is_a_vector(t: Tuple): Boolean = {
  !tuple_is_a_point(t)
}

def point(x: Double, y: Double, z: Double): Tuple = {
  Tuple(x, y, z, 1.0)
}

def vector(x: Double, y: Double, z: Double): Tuple = {
  Tuple(x, y, z, 0.0)
}

// "The smaller the dot product, the larger the angle between the two vectors"
def dot(a: Tuple, b: Tuple): Double = {
  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
}

// Computes the vector that is perpendicular to both vectors a and b.
def cross(a: Tuple, b: Tuple): Tuple = {
  vector(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
}
