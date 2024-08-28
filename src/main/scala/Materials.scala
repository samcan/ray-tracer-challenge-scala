case class Material(
    color: Color = Color(1, 1, 1),
    ambient: Double = 0.1,
    diffuse: Double = 0.9,
    specular: Double = 0.9,
    shininess: Double = 200.0
)

def CalculateDiffuse(
    lightDotNormal: Double,
    effectiveColor: Color,
    material: Material
): Color = {
  if lightDotNormal < 0 then Color(0, 0, 0)
  else effectiveColor * material.diffuse * lightDotNormal
}

def CalculateSpecular(
    lightDotNormal: Double,
    directionToLight: Tuple,
    light: PointLight,
    eye: Tuple,
    normal: Tuple,
    material: Material
): Color = {
  if lightDotNormal < 0 then Color(0, 0, 0)
  else
    val reflectDotEye = dot(Reflect(negate(directionToLight), normal), eye)
    if reflectDotEye <= 0 then Color(0, 0, 0)
    else
      val factor = math.pow(reflectDotEye, material.shininess)
      light.intensity * material.specular * factor
}

def Lighting(
    material: Material,
    light: PointLight,
    position: Tuple,
    eye: Tuple,
    normal: Tuple
): Color = {
  val effectiveColor = material.color * light.intensity
  val directionToLight = normalize(light.position - position)
  val ambientColor = effectiveColor * material.ambient
  val lightDotNormal = dot(directionToLight, normal)

  val diffuseColor = CalculateDiffuse(lightDotNormal, effectiveColor, material)
  val specularColor =
    CalculateSpecular(
      lightDotNormal,
      directionToLight,
      light,
      eye,
      normal,
      material
    )

  ambientColor + diffuseColor + specularColor
}
