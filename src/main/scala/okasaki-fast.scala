
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

object MatrixErrors {
  def outOfBounds = error("out of bounds")
  def notImplemented = error("not implemented")
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
    else MatrixErrors.outOfBounds
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
      case _ => MatrixErrors.outOfBounds
    }
  }
  def updated(i: Int, j: Int, f: A => A): QuadTreeSquareMatrix[A] =
    QT_Succ(subm.updated(i/2, j/2, (q:Tup) =>
      (q, i%2, j%2) match {
        case ((a,b,c,d),0,0) => (f(a),b,c,d)
        case ((a,b,c,d),0,1) => (a,f(b),c,d)
        case ((a,b,c,d),1,0) => (a,b,f(c),d)
        case ((a,b,c,d),1,1) => (a,b,c,f(d))
        case _ => MatrixErrors.outOfBounds
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

object FastExpSquareMatrix {
  abstract class Vec[A] {
    def size: Int
    def sub(i: Int): A
  }
  sealed case class Empty[A]() extends Vec[A] {
    val size = 0
    def sub(i:Int) = MatrixErrors.outOfBounds
  }
  sealed case class Id[A](value: A) extends Vec[A] {
    val size = 1
    def sub(i:Int) = if(i == 0) value else MatrixErrors.outOfBounds
  }
  sealed case class Pr[V[A]<:Vec[A], W[A]<:Vec[A], A](v: V[A], w: W[A])
       extends Vec[A] {
         val size = v.size + w.size
         def sub(i:Int) =
           if(i < v.size) v.sub(i)
           else w.sub(i - v.size)
       }
  trait Pair[V[A]<:Vec[A], W[A]<:Vec[A]] {
    type T[A] = Pr[V,W,A]              // for partial type application
  }

  abstract class Square_[V[A]<:Vec[A], W[A]<:Vec[A], A] {
    def sub(i: Int, j: Int): A
  }

  case class Zero[V[A]<:Vec[A], W[A]<:Vec[A], A] (
    data: V[V[A]]
  ) extends Square_[V,W,A] {
    def sub(i: Int, j: Int): A = data.sub(j).sub(i)
  }
  case class Even[V[A]<:Vec[A], W[A]<:Vec[A], A] (
    next: Square_[V, Pair[W,W]#T, A]
  ) extends Square_[V,W,A] {
    def sub(i: Int, j: Int): A = next.sub(i,j)
  }
  case class Odd[V[A]<:Vec[A], W[A]<:Vec[A], A] (
    next: Square_[Pair[V,W]#T, Pair[W,W]#T, A]
  ) extends Square_[V,W,A] {
    def sub(i: Int, j: Int): A = next.sub(i,j)
  }

  type Square[A] = Square_[Empty,Id,A]


  trait Maker[V[_]] {                   // to maintain rank-2 poly
    def apply[A](x: A): V[A]
  }

  val mkE = new Maker[Empty] {
    def apply[A](x: A) = Empty()
  }

  val mkI = new Maker[Id] {
    def apply[A](x: A) = Id(x)
  }

  case class mkP[V[X] <: Vec[X], W[X] <: Vec[X]](mkv: Maker[V], mkw: Maker[W])
       extends Maker[Pair[V,W]#T] {
         def apply[A](x: A) = Pr(mkv(x), mkw(x))
       }

  def create_[V[X] <: Vec[X], W[X] <: Vec[X], A]
    (mkv: Maker[V], mkw: Maker[W], x:A, n:Int): Square_[V,W,A] =
      if(n == 0) Zero(mkv(mkv(x)))
      else if(n%2 == 0) Even(create_[V,Pair[W,W]#T,A](mkv, mkP(mkw,mkw), x, n/2))
      else Odd(create_[Pair[V,W]#T, Pair[W,W]#T, A](mkP(mkv,mkw), mkP(mkw,mkw), x, n/2))

  def create[A](x: A, n: Int) = create_[Empty,Id,A](mkE,mkI,x,n)

//  def mkE[A](x: A): Empty[A] = Empty()
//  def mkI[A](x: A): Id[A] = Id(x)
//  def mkP[V[A]<:Vec[A], W[A]<:Vec[A], A]
//    (mkv: A=>V[A], mkw: A=>W[A], x: A): Pr[V,W,A] =
//      Pr(mkv(x), mkw(x))

}
