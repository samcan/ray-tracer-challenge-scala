def Clamp[A](minimum: A, maximum: A)(input: A)(implicit num: Numeric[A]): A = {
  num.max(num.min(input, maximum), minimum)
}
