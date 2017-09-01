package day0
import Monoid._

trait MonoidOp[A] {
  val m: Monoid[A]
  val v: A
  def |+|(v2: A) : A = m.mappend(v, v2)
}

object MonoidOp {

  implicit def toMonoidOp[A: Monoid](a: A) = new MonoidOp[A] {
    override val m = implicitly[Monoid[A]]
    override val v = a
  }
}

object pimpTest {
  import MonoidOp._
  def main(args: Array[String]): Unit = {


    println(1 |+| 2)

    println("pesho" |+| " " |+| "gosho")


    println(Vector(1,2) |+| Vector(3,4,5,6))
  }
}