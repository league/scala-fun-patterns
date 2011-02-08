
trait Matrix[A] extends Function2[Int,Int,A] {
  def width: Int
  def height: Int
  def size: (Int,Int) = (width, height)
  def apply(coord: (Int,Int)): A = apply(coord._1, coord._2)
  def updated(i: Int, j: Int, elem: A): Matrix[A]
}

trait MatrixFactory[M[A] <: Matrix[A]] {
  def fill[A](n1: Int, n2: Int)(elem: => A): M[A]
  def tabulate[A](n1: Int, n2: Int)(f:(Int,Int) => A): M[A]
}

trait SquareMatrix[A] extends Matrix[A] {
  def dimension: Int
  override def width: Int = dimension
  override def height: Int = dimension
}

trait SquareMatrixFactory[M[A] <: SquareMatrix[A]]
extends MatrixFactory[M] {
  def fill[A](n: Int)(elem: => A): M[A]
  def fill[A](n1: Int, n2: Int)(elem: => A): M[A] = {
    require(n1 == n2)
    fill(n1)(elem)
  }
  def tabulate[A](n: Int)(f:(Int,Int) => A): M[A]
  def tabulate[A](n1: Int, n2: Int)(f:(Int,Int) => A): M[A] = {
    require(n1 == n2)
    tabulate(n1)(f)
  }
}

abstract class QuadTreeSquareMatrix[A] extends SquareMatrix[A] {
  def exponent: Int
  override def dimension = 1 << exponent
}

case class QT_Zero[A](data: A) extends QuadTreeSquareMatrix[A] {
  val exponent: Int = 0
  override def toString: String = "Zero " + data.toString
  def apply(i: Int, j: Int): A =
    if(i==0 && j==0) data
    else error("out of bounds")
  def updated(i: Int, j: Int, elem: A): QuadTreeSquareMatrix[A] = {
    error("not implemented")
  }
}

case class QT_Succ[A](
  subm: QuadTreeSquareMatrix[Tuple4[A,A,A,A]]
) extends QuadTreeSquareMatrix[A] {
  val exponent = 1 + subm.exponent
  override def toString: String= "Succ " + subm.toString
  def apply(i: Int, j: Int): A = {
    val q = subm(i/2, j/2)
    (i%2, j%2) match {
      case (0,0) => q._1
      case (0,1) => q._2
      case (1,0) => q._3
      case (1,1) => q._4
      case _ => error("out of bounds")
    }
  }
  def updated(i: Int, j: Int, elem: A): QuadTreeSquareMatrix[A] = {
    error("not implemented")
  }
}

object QuadTreeSquareMatrix extends SquareMatrixFactory[QuadTreeSquareMatrix] {
  def fill[A](n: Int)(elem: => A): QuadTreeSquareMatrix[A] = {
    if(n == 1) QT_Zero(elem)
    else {
      require(n > 0 && n % 2 == 0)
      QT_Succ(fill(n/2, n/2)(elem,elem,elem,elem))
    }
  }
  def tabulate[A](n:Int)(f:(Int,Int)=>A): QuadTreeSquareMatrix[A] = {
    if(n == 1) QT_Zero(f(0,0))
    else {
      require(n > 0 && n % 2 == 0)
      QT_Succ(tabulate(n/2, n/2) { (i,j) =>
        (f(2*i, 2*j),
         f(2*i, 2*j+1),
         f(2*i+1, 2*j),
         f(2*i+1, 2*j+1))})
    }
  }
}

