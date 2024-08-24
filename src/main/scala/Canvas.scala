def MakeCanvasRow(cols: Int): IndexedSeq[Color] = {
  Vector.fill(cols)(Color(0, 0, 0)).toIndexedSeq
}

def MakeCanvas(rows: Int, cols: Int): IndexedSeq[IndexedSeq[Color]] = {
  Vector.fill(rows)(MakeCanvasRow(cols)).toIndexedSeq
}

def PixelAt(
    canvas: IndexedSeq[IndexedSeq[Color]],
    row: Int,
    col: Int
): Option[Color] = {
  if row < canvas.length && col < canvas(row).length then Some(canvas(row)(col))
  else None
}

def WritePixel(
    canvas: IndexedSeq[IndexedSeq[Color]],
    row: Int,
    col: Int,
    c: Color
): IndexedSeq[IndexedSeq[Color]] = {
  val updatedRow = canvas(row).updated(col, c)
  canvas.updated(row, updatedRow)
}
