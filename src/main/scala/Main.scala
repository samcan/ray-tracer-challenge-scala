import org.scalactic._
import org.scalactic.TripleEquals._

val EPSILON = 0.00001

implicit val doubleEquality: Equality[Double] =
  TolerantNumerics.tolerantDoubleEquality(EPSILON)

implicit val matrixEquality: Equality[IndexedSeq[IndexedSeq[Double]]] =
  new Equality[IndexedSeq[IndexedSeq[Double]]] {
    def areEqual(
        a: IndexedSeq[IndexedSeq[Double]],
        b: Any
    ): Boolean = {
      b match {
        case m: IndexedSeq[IndexedSeq[Double]] =>
          a.flatten
            .zip(m.flatten)
            .map((value1, value2) => value1 === value2)
            .foldLeft(true)((result, b) => result && b)
        case _ => false
      }
    }
  }

implicit val tupleEquality: Equality[IndexedSeq[Double]] =
  new Equality[IndexedSeq[Double]] {
    def areEqual(
        a: IndexedSeq[Double],
        b: Any
    ): Boolean = {
      b match {
        case m: IndexedSeq[Double] =>
          a.zip(m)
            .map((value1, value2) => value1 === value2)
            .foldLeft(true)((result, b) => result && b)
        case _ => false
      }
    }
  }

@main def hello(): Unit =
  println("Hello world!")
