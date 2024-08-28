def MakeCanvas(
    prefill: Color
)(x: Int, y: Int): Array[Array[Color]] = {
  Array.fill[Color](y, x)(prefill)
}

def MakeCanvas(x: Int, y: Int): Array[Array[Color]] = {
  MakeCanvas(Color(0, 0, 0))(x, y)
}

def PixelAt(
    canvas: Array[Array[Color]],
    x: Int,
    y: Int
): Option[Color] = {
  if y < canvas.length && x < canvas(y).length then Some(canvas(y)(x))
  else None
}

def SetPixel(
    canvas: Array[Array[Color]],
    x: Int,
    y: Int,
    c: Color
): Array[Array[Color]] = {
  canvas(y)(x) = c
  canvas
}
