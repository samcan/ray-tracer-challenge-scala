case class Sphere(
    id: java.util.UUID = java.util.UUID.randomUUID(),
    transform: IndexedSeq[IndexedSeq[Double]] = IdentityMatrix()
)

def Intersect(s: Sphere, r: Ray): Seq[Intersection] = {
  val transformedRay = Transform(r, Inverse(Some(s.transform)).get)
  // the vector from the sphere's center to the ray's origin
  // assume the sphere is centered at the world's origin
  val vectorSphereToRay = transformedRay.origin - point(0, 0, 0)

  val a = dot(transformedRay.direction, transformedRay.direction)
  val b = 2 * dot(transformedRay.direction, vectorSphereToRay)
  val c = dot(vectorSphereToRay, vectorSphereToRay) - 1

  val discriminant = math.pow(b, 2) - 4 * a * c

  if discriminant < 0 then List[Intersection]()
  else
    List[Intersection](
      Intersection((-b - math.sqrt(discriminant)) / (2 * a), s.id),
      Intersection((-b + math.sqrt(discriminant)) / (2 * a), s.id)
    )
}
