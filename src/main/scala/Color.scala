case class Color(red: Double, green: Double, blue: Double) {
  def +(b: Color): Color = {
    Color(this.red + b.red, this.green + b.green, this.blue + b.blue)
  }

  def -(b: Color): Color = {
    Color(this.red - b.red, this.green - b.green, this.blue - b.blue)
  }

  // Blend two colors together by computing the Hadamard product
  def *(b: Any): Color = {
    b match {
      case Color(red, green, blue) =>
        Color(this.red * red, this.green * green, this.blue * blue)
      case d: Double => Color(this.red * d, this.green * d, this.blue * d)
    }
  }

  def ~=(b: Color): Boolean = {
    math.abs(this.red - b.red) < EPSILON && math.abs(
      this.green - b.green
    ) < EPSILON && math.abs(this.blue - b.blue) < EPSILON
  }
}
