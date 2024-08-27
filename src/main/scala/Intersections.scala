case class Intersection(t: Double, objectId: java.util.UUID)

def Hit(xs: Seq[Intersection]): Option[Intersection] = {
  xs.sortBy(i => (i.t, i.objectId)).find(i => i.t >= 0)
}
