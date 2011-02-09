
trait Matrix[A] extends Function2[Int,Int,A] {
  def width: Int
  def height: Int
  def size: (Int,Int) = (width, height)
  def apply(coord: (Int,Int)): A = apply(coord._1, coord._2)
  def updated(i: Int, j: Int, elem: A): Matrix[A]
  def map[B](f: A => B): Matrix[B]
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

trait Quad[A] {
  type Tup = (A,A,A,A)
  def exponent: Int
  def dimension = 1 << exponent
}

abstract class QuadTreeSquareMatrix[A]
extends SquareMatrix[A] with Quad[A] {
  def updated(i: Int, j: Int, f: A => A): QuadTreeSquareMatrix[A]
  def updated(i: Int, j: Int, elem: A): QuadTreeSquareMatrix[A] =
    updated(i, j, (_:A) => elem)
  def map[B](f: A => B): QuadTreeSquareMatrix[B]
}

case class QT_Zero[A](data: A) extends QuadTreeSquareMatrix[A] {
  val exponent: Int = 0
  override def toString: String = "Zero " + data.toString
  def apply(i: Int, j: Int): A =
    if(i==0 && j==0) data
    else error("out of bounds")
  def updated(i: Int, j: Int, f: A => A): QuadTreeSquareMatrix[A] =
    QT_Zero(f(data))
  def map[B](f: A => B): QuadTreeSquareMatrix[B] =
    QT_Zero(f(data))
}

case class QT_Succ[A](
  subm: QuadTreeSquareMatrix[Quad[A]#Tup]
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
  def updated(i: Int, j: Int, f: A => A): QuadTreeSquareMatrix[A] =
    QT_Succ(subm.updated(i/2, j/2, (q:Tup) =>
      (q, i%2, j%2) match {
        case ((a,b,c,d),0,0) => (f(a),b,c,d)
        case ((a,b,c,d),0,1) => (a,f(b),c,d)
        case ((a,b,c,d),1,0) => (a,b,f(c),d)
        case ((a,b,c,d),1,1) => (a,b,c,f(d))
        case _ => error("out of bounds")
      }))
  def map[B](f: A => B): QuadTreeSquareMatrix[B] =
    QT_Succ(subm.map {
      q:Tup => (f(q._1), f(q._2), f(q._3), f(q._4))
    })
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
