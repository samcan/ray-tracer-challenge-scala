val MIN_COLOR_VAL: Long = 0
val MAX_COLOR_VAL: Long = 255
val MAX_ROW_LEN = 70

def CanvasToPPM(canvas: Array[Array[Color]]): Seq[String] = {
  val rows = canvas.length
  val cols = canvas(rows - 1).length

  val header = Seq[String]("P3\n", s"$rows $cols\n", s"$MAX_COLOR_VAL\n")
  val data = canvas.map(WriteRow)
  (header ++ data)
}

def WritePixel(pixel: Color): String = {
  val clamper = Clamp(MIN_COLOR_VAL, MAX_COLOR_VAL)
  s"${clamper(math.round(pixel.red * MAX_COLOR_VAL))} ${clamper(
      math.round(pixel.green * MAX_COLOR_VAL)
    )} ${clamper(math.round(pixel.blue * MAX_COLOR_VAL))}"
}

def WriteRow(row: Array[Color]): String = {
  val output = row
    .map(WritePixel)
    .foldLeft("")((output, s) => output + s + " ")
    .trim()
    .concat("\n")

  if output.length > MAX_ROW_LEN then
    output.patch(output.lastIndexWhere(c => c == ' ', MAX_ROW_LEN), "\n", 1)
  else output
}
