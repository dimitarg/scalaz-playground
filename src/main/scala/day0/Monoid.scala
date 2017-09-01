package day0

trait Monoid[A] {
  def mappend(x: A, y: A) : A
  def mzero: A
}

object Monoid {

  implicit val intSumMonoid: Monoid[Int] = new Monoid[Int] {
    override def mappend(x: Int, y: Int): Int = x + y
    override def mzero: Int = 0
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def mappend(x: String, y: String): String = x + y
    override def mzero: String = ""
  }

  implicit def vecMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    override def mappend(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

    override def mzero: Vector[A] = Vector()
  }
}

object Tst {

  def sum[A](xs: List[A])(implicit m: Monoid[A]) : A = xs.foldLeft(m.mzero)(m.mappend)

  def sumFancy[A: Monoid](xs: List[A]) : A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mappend)
  }

  def main(args: Array[String]): Unit = {
    println(sumFancy(List(1,2,3)))

    println(sumFancy(List("a", "bc", "d")))

    println(sumFancy(List(2,3,4))(new Monoid[Int] {
      override def mappend(x: Int, y: Int): Int = x * y

      override def mzero: Int = 1
    }))
  }
}
