
trait uList[A]
case class uCons[A](hd: A, tl: uList[A]) extends uList[A]
case class uNil[A]() extends uList[A]

trait Weird[A]
case class Wons[A](hd: A, tl: Weird[(A,A)]) extends Weird[A]
case class Wil[A]() extends Weird[A]

object Nested {
val z: Weird[Int] = Wons(1, Wil[I2]())
val y: Weird[Int] = Wons(1, Wons((2,3), Wil[I4]))
val x: Weird[Int] = Wons(1, Wons((2,3), Wons(((4,5),(6,7)), Wil[I8]())))

type I2 = (Int,Int); type I4 = (I2,I2); type I8 = (I4,I4)

}
