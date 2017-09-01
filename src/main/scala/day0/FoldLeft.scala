package day0

import day0.Monoid._


// this ended up bit different from the tutorial since i required an implicit monoid instance instead of passing zero and sum
// probably a bad decision, too


trait FoldLeft[F[_]] {
  def foldLeft[A](x: F[A])(implicit m: Monoid[A]) : A
}

object FoldLeft {
  implicit val listFold: FoldLeft[List] = new FoldLeft[List] {
    override def foldLeft[A](xs: List[A])(implicit m: Monoid[A]): A = xs.foldLeft(m.mzero)(m.mappend)
  }
}

object FLTest {

  def sumIt[F[_]: FoldLeft, A: Monoid](xs: F[A]) : A = {
    val f = implicitly[FoldLeft[F]]
    f.foldLeft(xs)
  }

  def main(args: Array[String]): Unit = {

    println(implicitly[FoldLeft[List]].foldLeft(List(1,2,3)))
    println(sumIt(List(1,2,3,4)))

  }
}
