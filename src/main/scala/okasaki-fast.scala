
/** Operations on general 2D matrices. */
trait Matrix[A] extends Function2[Int,Int,A] {
  type M[X] <: Matrix[X]
  def width: Int
  def height: Int
  def size: (Int,Int) = (width, height)
  def apply(coord: (Int,Int)): A = apply(coord._1, coord._2)
  def updated(i: Int, j: Int, elem: A): M[A]
  def map[B](f: A => B): M[B]
}

/** Constructors for general 2D matrices. */
trait MatrixFactory {
  type M[X] <: Matrix[X]
  def fill[A](n1: Int, n2: Int)(elem: => A): M[A]
  def tabulate[A](n1: Int, n2: Int)(f:(Int,Int) => A): M[A]
}

/** A square matrix has a single dimension (i.e., width == height). */
trait SquareMatrix[A] extends Matrix[A] {
  type M[X] <: SquareMatrix[X]
  def dimension: Int
  override def width: Int = dimension
  override def height: Int = dimension
}

/** Constructors for square matrices. */
trait SquareMatrixFactory extends MatrixFactory {
  type M[X] <: SquareMatrix[X]
  def fill[A](n: Int)(elem: => A): M[A] =
    tabulate(n){(i,j) => elem}
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

/** Standardizes the errors raised by matrix implementations. */
object MatrixErrors {
  def outOfBounds = error("out of bounds")
  def notImplemented = error("not implemented")
}

object QuadTreeSquareMatrix extends SquareMatrixFactory {

  type M[A] = QT_Matrix[A]

  sealed abstract class QT_Matrix[A] extends SquareMatrix[A] {
    type M[X] = QT_Matrix[X]
    def exponent: Int
    def dimension = 1 << exponent
    def updated(i: Int, j: Int, f: A => A): M[A]
    def updated(i: Int, j: Int, elem: A): M[A] =
      updated(i, j, (_:A) => elem)
  }

  private case class Zero[A](data: A) extends QT_Matrix[A] {
    val exponent: Int = 0
    override def toString: String = "Zero " + data.toString
    def apply(i: Int, j: Int): A =
      if(i==0 && j==0) data
      else MatrixErrors.outOfBounds
    def updated(i: Int, j: Int, f: A => A): M[A] = Zero(f(data))
    def map[B](f: A => B): M[B] = Zero(f(data))
  }

  type Quad[A] = (A,A,A,A)

  private case class Succ[A](next: QT_Matrix[Quad[A]]) extends QT_Matrix[A] {
    val exponent = 1 + next.exponent
    override def toString: String= "Succ " + next.toString
    def apply(i: Int, j: Int): A = (next(i/2, j/2), i%2, j%2) match {
      case ((q,_,_,_),0,0) => q
      case ((_,q,_,_),0,1) => q
      case ((_,_,q,_),1,0) => q
      case ((_,_,_,q),1,1) => q
      case _ => MatrixErrors.outOfBounds
    }
    def updated(i: Int, j: Int, f: A => A): M[A] =
      Succ(next.updated(i/2, j/2, q => (q, i%2, j%2) match {
        case ((a,b,c,d),0,0) => (f(a),  b ,  c ,  d )
        case ((a,b,c,d),0,1) => (  a ,f(b),  c ,  d )
        case ((a,b,c,d),1,0) => (  a ,  b ,f(c),  d )
        case ((a,b,c,d),1,1) => (  a ,  b ,  c ,f(d))
        case _ => MatrixErrors.outOfBounds
      }))
    def map[B](f: A => B): M[B] =
      Succ(next.map {
        case (a,b,c,d) => (f(a), f(b), f(c), f(d))
      })
  }

  def tabulate[A](n:Int)(f:(Int,Int)=>A): M[A] = {
    if(n == 1) Zero(f(0,0))
    else {
      require(n > 0 && n % 2 == 0)
      Succ(tabulate(n/2, n/2) { (i,j) =>
        (f(2*i, 2*j),
         f(2*i, 2*j+1),
         f(2*i+1, 2*j),
         f(2*i+1, 2*j+1))})
    }
  }
}

object SizedVectors {
  /* Could implement these with case classes, but want to keep them
     very lightweight: just nested pairs with no superfluous
     wrappers. */
  type Empty[A] = Unit
  type Id[A] = A
  type Two[V[_], W[_], A] = Pair[V[A], W[A]]

  /** Partial application of `Two` type constructor. */
  trait Tw[V[_], W[_]] {
    type T[A] = Two[V,W,A]
  }

  /** Constructors for sized vectors.  Written in this form (rather
      than just functions) to maintain [rank-2] polymorphism when
      passing them as arguments. */
  trait Maker[V[_]] {
    def apply[A](f: Int => A): V[A]
  }

  val mkE = new Maker[Empty] {
    def apply[A](f: Int => A): Empty[A] = ()
  }

  val mkI = new Maker[Id] {
    def apply[A](f: Int => A): Id[A] = f(0)
  }

  case class mkP[V[_], W[_]](mkv: Maker[V], mkw: Maker[W], vsize: Int)
       extends Maker[Tw[V,W]#T] {
         def apply[A](f: Int => A): Two[V,W,A] = (mkv(f), mkw(i => f(i + vsize)))
       }

  /* Sized vector subscript functions. */
  def subE[A](i: Int, e: Empty[A]): A = MatrixErrors.outOfBounds

  def subI[A](i: Int, x: Id[A]): A =
    if(i == 0) x
    else MatrixErrors.outOfBounds

  def subP[V[_], W[_], A]
    (subv: (Int,V[A])=>A, subw: (Int,W[A])=>A, vsize: Int)
    (i: Int, p: Two[V,W,A]) =
       if(i < vsize) subv(i, p._1)
       else subw(i-vsize, p._2)

  /* Some examples: singletons represented as
   * fastexp E I 1 = fastexp Tw[E,I] Tw[I,I] 0 = Tw[E,I]
   */
  type T1[A] = Two[Empty,Id,A]
  def mk1[A](f: Int => A): T1[A] = mkP(mkE,mkI,0)(f)
  def sub1[A](i: Int, v: T1[A]): A = subP(subE, subI[A], 0)(i, v)
  assert(sub1(0,mk1{x => 'Q'}) == 'Q')

  /* Pairs represented as
   * fastexp E I 2 = fastexp E Tw[I,I] 1 = fastexp Tw[E,Tw[I,I]] _ 0 = Tw[E,Tw[I,I]]
   */
  type T2[A] = Two[Empty,Tw[Id,Id]#T,A]
  def mk2[A](f: Int => A): T2[A] = mkP[Empty,Tw[Id,Id]#T](mkE,mkP(mkI,mkI,1),0)(f)
  def sub2[A](i: Int, v: T2[A]): A =
    subP[Empty,Tw[Id,Id]#T,A](subE, subP[Id,Id,A](subI, subI, 1), 0)(i,v)
//  assert(sub2(0,mk2{x => x
}

/*
object FastExpSquareMatrix {

  type Empty[A] = Unit
  type Id[A] = A
  type Two[V[_], W[_], A] = Pair[V[A], W[A]]

  trait Tw[V[_], W[_]] {                // partial application of Two
    type T[A] = Two[V,W,A]
  }



  type Square[A] = Square_[Empty,Id,A]

  sealed abstract class Square_[V[_], W[_], A] extends SquareMatrix[A]

  case class FE_Zero[V[_], W[_], A] private (data: V[V[A]])
       extends Square_[V,W,A]
  {
    def dimension: Int = MatrixErrors.notImplemented
    def apply(i: Int, j: Int): A = MatrixErrors.notImplemented
    def map[B](f: A => B): Square[B] = MatrixErrors.notImplemented
    def updated(i: Int, j: Int, elem: A): Square[A] = MatrixErrors.notImplemented
  }

  case class FE_Even[V[_], W[_], A] private (next: Square_[V, Tw[W,W]#T, A])
       extends Square_[V,W,A]
  {
    def dimension: Int = MatrixErrors.notImplemented
    def apply(i: Int, j: Int): A = MatrixErrors.notImplemented
    def map[B](f: A => B): Square[B] = MatrixErrors.notImplemented
    def updated(i: Int, j: Int, elem: A): Square[A] = MatrixErrors.notImplemented
  }

  case class FE_Odd[V[_], W[_], A] private (next: Square_[Tw[V,W]#T, Tw[W,W]#T, A])
       extends Square_[V,W,A]
  {
    def dimension: Int = MatrixErrors.notImplemented
    def apply(i: Int, j: Int): A = MatrixErrors.notImplemented
    def map[B](f: A => B): Square[B] = MatrixErrors.notImplemented
    def updated(i: Int, j: Int, elem: A): Square[A] = MatrixErrors.notImplemented
  }

/*
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
*/
}
*/
