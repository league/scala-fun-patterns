
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
  trait Getter[V[_]] {
    def apply[A](i:Int, v: V[A]): A
  }

  val subE = new Getter[Empty] {
    def apply[A](i: Int, v: Empty[A]): A = MatrixErrors.outOfBounds
  }

  val subI = new Getter[Id] {
    def apply[A](i: Int, v: Id[A]): A =
      if(i == 0) v
      else MatrixErrors.outOfBounds
  }

  case class subP[V[_],W[_]](subv: Getter[V], subw: Getter[W], vsize: Int)
       extends Getter[Tw[V,W]#T] {
         def apply[A](i: Int, v: Two[V,W,A]): A =
           if(i < vsize) subv(i, v._1)
           else subw(i-vsize, v._2)
       }

  /* For convenience and testing, here are some specializations for
     small lengths. */
  trait VectorSpec {
    type T[_]
    def size: Int
    def apply[A](f: Int => A): T[A]
    def sub[A](v: T[A], i: Int): A
  }

  object One extends VectorSpec {
    /* fastexp E I 1 = fastexp Tw[E,I] Tw[I,I] 0 = Tw[E,I] */
    type T[A] = Two[Empty,Id,A]
    def apply[A](f: Int => A): T[A] = mkP(mkE,mkI,0)(f)
    def sub[A](v: T[A], i: Int): A = subP(subE, subI, 0)(i, v)
    def size = 1
  }
  object Two extends VectorSpec {
    /* fastexp E I 2 =
     * fastexp E Tw[I,I] 1 =
     * fastexp Tw[E,Tw[I,I]] _ 0 =
     * Tw[E,Tw[I,I]]
     */
    type T[A] = Two[Empty,Tw[Id,Id]#T,A]
    def apply[A](f: Int => A): T[A] =
      mkP[Empty,Tw[Id,Id]#T](mkE,mkP(mkI,mkI,1),0)(f)
    def sub[A](v: T[A], i: Int): A =
      subP[Empty,Tw[Id,Id]#T](subE, subP(subI, subI, 1), 0)(i,v)
    def size = 2
  }
}

object FastExpSquareMatrix extends SquareMatrixFactory {
  import SizedVectors._

  case class FE_Matrix[A](m: FE_M[Empty,Id,A])
  extends SquareMatrix[A] {
    type M[A] = FE_Matrix[A]
    def dimension = m.dimension
    def apply(i: Int, j: Int): A = m.sub(subE,subI,0,1,i,j)
    def map[B](f: A => B) = MatrixErrors.notImplemented
    def updated(i:Int, j: Int, elem: A): M[A] =
      MatrixErrors.notImplemented
    override def toString = m.toString
  }

  type M[A] = FE_Matrix[A]

  sealed abstract class FE_M[V[_],W[_],A] {
    def dimension: Int
    def sub(subv: Getter[V], subw: Getter[W],
            vsize: Int, wsize: Int, i: Int, j: Int): A
  }

  /*private*/ case class Zero[V[_],W[_],A](data: V[V[A]])
  extends FE_M[V,W,A] {
    val dimension = 0
    def sub(subv: Getter[V], subw: Getter[W],
            vsize: Int, wsize: Int, i: Int, j: Int): A =
      subv(j, subv(i, data))
    override def toString: String = "Zero " + data.toString
  }

  /*private*/ case class Odd[V[_],W[_],A]
      (next: FE_M[Tw[V,W]#T, Tw[W,W]#T, A])
  extends FE_M[V,W,A] {
    val dimension = next.dimension*2 + 1
    def sub(subv: Getter[V], subw: Getter[W],
            vsize: Int, wsize: Int, i: Int, j: Int): A =
      next.sub(subP(subv,subw,vsize),
               subP(subw,subw,wsize),
               vsize+wsize, wsize+wsize, i, j)
    override def toString: String = "Odd " + next.toString
  }

  /*private*/ case class Even[V[_],W[_],A]
      (next: FE_M[V, Tw[W,W]#T, A])
  extends FE_M[V,W,A] {
    val dimension = next.dimension*2
    def sub(subv: Getter[V], subw: Getter[W],
            vsize: Int, wsize: Int, i: Int, j: Int): A =
      next.sub(subv, subP(subw,subw,wsize),
               vsize, wsize*2, i, j)
    override def toString: String = "Even " + next.toString
  }

  def oneByOne[A](f:(Int,Int) => A) =
    FE_Matrix(
      Odd[Empty,Id,A](Zero[Tw[Empty,Id]#T, Tw[Id,Id]#T, A](
        One(i => One.apply[A](j => f(i,j))))))

  def tabulate[A](n: Int)(f:(Int,Int) => A): M[A] = {
    def loop[V[_],W[_]](mkv: Maker[V], mkw: Maker[W],
                        vsize: Int, wsize: Int, k: Int): FE_M[V,W,A] =
      if(k == 0)
        Zero(mkv(i => mkv(j => f(i,j))))
      else if(k%2 == 1)
        Odd(loop[Tw[V,W]#T, Tw[W,W]#T](
          mkP(mkv,mkw,vsize), mkP(mkw,mkw,wsize),
          vsize+wsize, wsize*2, k/2))
      else
        Even(loop[V, Tw[W,W]#T](
          mkv, mkP(mkw,mkw,wsize),
          vsize, wsize*2, k/2))
    FE_Matrix(loop(mkE, mkI, 0, 1, n))
  }
}
