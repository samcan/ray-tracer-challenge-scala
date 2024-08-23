case class Color(red: Double, green: Double, blue: Double) {
  def +(b: Color): Color = {
    Color(this.red + b.red, this.green + b.green, this.blue + b.blue)
  }

  def -(b: Color): Color = {
    Color(this.red - b.red, this.green - b.green, this.blue - b.blue)
  }

  def *(b: Color): Color = {
    Color(this.red * b.red, this.green * b.green, this.blue * b.blue)
  }

  def ~=(b: Color): Boolean = {
    math.abs(this.red - b.red) < EPSILON && math.abs(
      this.green - b.green
    ) < EPSILON && math.abs(this.blue - b.blue) < EPSILON
  }
}
