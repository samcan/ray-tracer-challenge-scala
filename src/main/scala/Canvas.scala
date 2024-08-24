def MakeCanvasRow(x: Int): IndexedSeq[Color] = {
  Vector.fill(x)(Color(0, 0, 0)).toIndexedSeq
}

def MakeCanvas(x: Int, y: Int): IndexedSeq[IndexedSeq[Color]] = {
  Vector.fill(y)(MakeCanvasRow(x)).toIndexedSeq
}

def PixelAt(
    canvas: IndexedSeq[IndexedSeq[Color]],
    x: Int,
    y: Int
): Option[Color] = {
  if y < canvas.length && x < canvas(y).length then Some(canvas(y)(x))
  else None
}

def SetPixel(
    canvas: IndexedSeq[IndexedSeq[Color]],
    x: Int,
    y: Int,
    c: Color
): IndexedSeq[IndexedSeq[Color]] = {
  val updatedRow = canvas(y).updated(x, c)
  canvas.updated(y, updatedRow)
}
