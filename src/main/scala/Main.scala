@main def hello(): Unit =
  println("Hello world!")

case class Tuple(x: Double, y: Double, z: Double, w: Double)

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
