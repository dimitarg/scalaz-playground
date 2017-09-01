package day0


trait Plus[A] {
  def plus(x: A, y: A) : A
}

object PlusInstances {
  implicit val intPlus: Plus[Int] = (x, y) => x + y
}


object AdHoc {

  def plus[A: Plus](x: A, y: A) = implicitly[Plus[A]].plus(x, y)

}
