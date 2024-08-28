case class Intersection(t: Double, obj: Sphere)

def Hit(xs: Seq[Intersection]): Option[Intersection] = {
  xs.sortBy(i => i.t).find(i => i.t >= 0)
}
