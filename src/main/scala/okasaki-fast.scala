/* Scala code for Chris Okasaki's 1999 ICFP paper,
 * "From Fast Exponentiation to Square Matrices"
 *
 * Copyright ©2011 Christopher League <league@contrapunctus.net>
 * Creative Commons Attribution License
 */

/** Operations on general 2D matrices. */
trait Matrix[A] extends Function2[Int,Int,A] {
  type M[X] <: Matrix[X]
  def width: Int
  def height: Int
  def size: (Int,Int) = (width, height)
  def apply(coord: (Int,Int)): A = apply(coord._1, coord._2)
  def updated[B>:A](i: Int, j: Int, elem: B): M[B]
  def map[B](f: (Int,Int,A) => B): M[B]
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

/** Square matrices based on quad trees.  Simpler to understand than
    fast-exponentiation version, but with the limitation that
    dimension must be a power of two. */

object QuadTreeSquareMatrix extends SquareMatrixFactory {

  type M[A] = QT_Matrix[A]

  sealed abstract class QT_Matrix[A] extends SquareMatrix[A] {
    type M[X] = QT_Matrix[X]
    def exponent: Int
    def dimension = 1 << exponent
    def updated[B>:A](i: Int, j: Int, f: A => B): M[B]
    def updated[B>:A](i: Int, j: Int, elem: B): M[B] =
      updated(i, j, (_:A) => elem)
  }

  private case class Zero[A](data: A) extends QT_Matrix[A] {
    val exponent: Int = 0
    override def toString: String = "Zero " + data.toString
    def apply(i: Int, j: Int): A =
      if(i==0 && j==0) data
      else MatrixErrors.outOfBounds
    def updated[B>:A](i: Int, j: Int, f: A => B): M[B] = Zero(f(data))
    def map[B](f: (Int,Int,A) => B): M[B] = Zero(f(0,0,data))
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
    def updated[B>:A](i: Int, j: Int, f: A => B): M[B] =
      Succ(next.updated(i/2, j/2, q => (q, i%2, j%2) match {
        case ((a,b,c,d),0,0) => (f(a),  b ,  c ,  d )
        case ((a,b,c,d),0,1) => (  a ,f(b),  c ,  d )
        case ((a,b,c,d),1,0) => (  a ,  b ,f(c),  d )
        case ((a,b,c,d),1,1) => (  a ,  b ,  c ,f(d))
        case _ => MatrixErrors.outOfBounds
      }))
    def map[B](f: (Int,Int,A) => B): M[B] =
      Succ(next.map {
        case (i,j,(a,b,c,d)) => (f(2*i, 2*j, a),
                                 f(2*i, 2*j+1, b),
                                 f(2*i+1, 2*j, c),
                                 f(2*i+1, 2*j+1, d))
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

/** Logarithmic representations of vectors that encode the size as
    part of the type. */
object SizedVectors {
  /* Could implement these with case classes, but want to keep them
     very lightweight: just nested pairs with no superfluous
     wrappers. */
  type Empty[+A] = Unit
  type Id[+A] = A
  type Pair[V[+_], W[+_], +A] = (V[A], W[A])

  /** Partial application of `Pair` type constructor: Pr[V,W]#T */
  trait Pr[V[+_], W[+_]] {
    type T[+A] = Pair[V,W,A]
  }

  /** Interface for a package of operations on a fixed-size vector. */
  trait Ops[TT[+_]] {
    type T[+A] = TT[A]
    def size: Int
    def apply[A](f: Int => A): T[A]     // constructor
    def sub[A](v: T[A], i: Int): A
    def map[A,B](v: T[A], f: (Int,A) => B): T[B]
    def update[A,B>:A](v: T[A], i: Int, x: B): T[B]
  }

  /** Implementations for the empty vector. */
  val opsE = new Ops[Empty] {
    val size = 0
    def apply[A](f: Int => A) = ()
    def sub[A](v: T[A], i:Int) = MatrixErrors.outOfBounds
    def map[A,B](v: T[A], f: (Int,A) => B) = ()
    def update[A,B>:A](v: T[A], i: Int, x: B) = ()
  }

  /** Implementations for the singleton vector. */
  val opsI = new Ops[Id] {
    val size = 1
    def apply[A](f: Int => A) = f(0)
    def sub[A](v: T[A], i:Int) =
      if(i == 0) v else MatrixErrors.outOfBounds
    def map[A,B](v: T[A], f: (Int,A) => B) = f(0,v)
    def update[A,B>:A](v: T[A], i: Int, x: B) = sub(x,i)
  }

  /** Construct implementation based on a pair of smaller vectors.
      Larger sizes will be composed using the fast-exponentiation
      analogy. */
  case class opsP[V[+_],W[+_]](opsV: Ops[V], opsW: Ops[W])
  extends Ops[Pr[V,W]#T] {
    val size = opsV.size + opsW.size
    def apply[A](f: Int => A) =
      (opsV(f), opsW(i => f(i + opsV.size)))
    def sub[A](v: T[A], i:Int) =
      if(i < opsV.size) opsV.sub(v._1, i)
      else opsW.sub(v._2, i - opsV.size)
    def map[A,B](v: T[A], f: (Int,A) => B) =
      (opsV.map(v._1, f),
       opsW.map(v._2, (i,a:A) => f(i + opsV.size, a)))
    def update[A,B>:A](v: T[A], i: Int, x: B) =
      if(i < opsV.size)
        (opsV.update(v._1, i, x), v._2)
      else
        (v._1, opsW.update(v._2, i - opsV.size, x))
  }

  /** Some encodings of small vectors, for testing.  Except for those
      suffixed with `a` (which are intermediate representations),
      these follow the fast exponentiation algorithm, which is
      summarized as:
         fastexp N = fastexp Empty Id N
      where
         fastexp’ V W 0 = V
         fastexp’ V W K = fastexp’ V (W,W) K/2  if K even
         fastexp’ V W K = fastexp’ (V,W) (W,W) K/2  if K odd
    */

  val ops1  = opsP(opsE,opsI)
  val ops2a = opsP(opsI,opsI)
  val ops2  = opsP[Empty,ops2a.T](opsE,ops2a)
  val ops3  = opsP[ops1.T,ops2a.T](ops1,ops2a)
  val ops4a = opsP[ops2a.T,ops2a.T](ops2a,ops2a)
  val ops4  = opsP[Empty,ops4a.T](opsE,ops4a)
  val ops5  = opsP[ops1.T,ops4a.T](ops1,ops4a)
  val ops6  = opsP[ops2.T,ops4a.T](ops2,ops4a)
  val ops7  = opsP[ops3.T,ops4a.T](ops3,ops4a)
  val ops8  = opsP[ops4.T,ops4a.T](ops4,ops4a)
}

/** Square matrices based on two-dimensional sized vectors.  The
    datatype will have a "header" of even/odd constructors, building
    up the types to the properly-sized vectors.  All the data appear
    beneath the `Zero` constructor. */

object FastExpSquareMatrix extends SquareMatrixFactory {
  sealed abstract class FE_Matrix[V[+_],W[+_],A]
  extends SquareMatrix[A] {
    type M[A] = FE_Matrix[V,W,A]
  }

  private case class Zero[V[+_],W[+_],A](
    data: V[V[A]],
    ops: SizedVectors.Ops[V])
  extends FE_Matrix[V,W,A] {
    val dimension = 0
    def apply(i: Int, j: Int): A =
      ops.sub(ops.sub(data, i), j)
    def updated[B>:A](i: Int, j: Int, elem: B) = {
      val before = ops.sub(data, i)
      val after = ops.update(before, j, elem)
      Zero(ops.update(data, i, after), ops)
    }
    def map[B](f: (Int,Int,A) => B): M[B] =
      Zero(ops.map(data, (i, row:V[A]) =>
        ops.map(row, (j, elt:A) => f(i,j,elt))), ops)
    override def toString: String = "Zero " + data.toString
  }

  private case class Odd[V[+_],W[+_],A](
    next: FE_Matrix[SizedVectors.Pr[V,W]#T, SizedVectors.Pr[W,W]#T, A])
  extends FE_Matrix[V,W,A] {
    val dimension = next.dimension*2 + 1
    def apply(i: Int, j: Int): A = next(i,j)
    def updated[B>:A](i: Int, j: Int, elem: B) =
      Odd(next.updated(i,j,elem))
    def map[B](f: (Int,Int,A) => B): M[B] =
      Odd(next.map(f))
    override def toString: String = "Odd " + next.toString
  }

  private case class Even[V[+_],W[+_],A](
    next: FE_Matrix[V, SizedVectors.Pr[W,W]#T, A])
  extends FE_Matrix[V,W,A] {
    val dimension = next.dimension*2
    def apply(i: Int, j: Int): A = next(i,j)
    def updated[B>:A](i: Int, j: Int, elem: B) =
      Even(next.updated(i,j,elem))
    def map[B](f: (Int,Int,A) => B): M[B] =
      Even(next.map(f))
    override def toString: String = "Even " + next.toString
  }

  type M[A] = FE_Matrix[SizedVectors.Empty, SizedVectors.Id,A]

  def tabulate[A](n: Int)(f:(Int,Int) => A): M[A] = {
    def loop[V[+_],W[+_]](opsV: SizedVectors.Ops[V],
                          opsW: SizedVectors.Ops[W],
                          k: Int): FE_Matrix[V,W,A] =
      if(k == 0)
        Zero(opsV(i => opsV(j => f(i,j))), opsV)
      else if(k%2 == 1)
        Odd(loop[SizedVectors.Pr[V,W]#T, SizedVectors.Pr[W,W]#T](
          SizedVectors.opsP(opsV,opsW),
          SizedVectors.opsP(opsW,opsW), k/2))
      else
        Even(loop[V, SizedVectors.Pr[W,W]#T](
          opsV, SizedVectors.opsP(opsW,opsW), k/2))
    loop(SizedVectors.opsE, SizedVectors.opsI, n)
  }
}
