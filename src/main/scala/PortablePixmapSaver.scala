val MIN_COLOR_VAL: Long = 0
val MAX_COLOR_VAL: Long = 255

def CanvasToPPM(canvas: IndexedSeq[IndexedSeq[Color]]): Seq[String] = {
  val rows = canvas.length
  val cols = canvas(rows - 1).length

  val header = Seq[String]("P3\n", s"$rows $cols\n", s"$MAX_COLOR_VAL")
  val data = canvas.map(WriteRow)
  header ++ data
}

def WritePixel(pixel: Color): String = {
  val clamper = Clamp(MIN_COLOR_VAL, MAX_COLOR_VAL)
  s"${clamper(math.round(pixel.red * MAX_COLOR_VAL))} ${clamper(
      math.round(pixel.green * MAX_COLOR_VAL)
    )} ${clamper(math.round(pixel.blue * MAX_COLOR_VAL))}"
}

def WriteRow(row: IndexedSeq[Color]): String = {
  row
    .map(WritePixel)
    .foldLeft("")((output, s) => output + s + " ")
    .trim()
    .concat("\n")
}
