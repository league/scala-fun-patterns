
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
  type Empty[+A] = Unit
  type Id[+A] = A
  type Pair[V[+_], W[+_], +A] = (V[A], W[A])

  /** Partial application of `Pair` type constructor: Pr[V,W]#T */
  trait Pr[V[+_], W[+_]] {
    type T[+A] = Pair[V,W,A]
  }

  trait Ops[TT[+_]] {
    type T[+A] = TT[A]
    def size: Int
    def apply[A](f: Int => A): T[A]     // constructor
    def sub[A](v: T[A], i: Int): A
    def map[A,B](v: T[A], f: (Int,A) => B): T[B]
    def update[A,B>:A](v: T[A], i: Int, x: B): T[B]
  }

  val opsE = new Ops[Empty] {
    val size = 0
    def apply[A](f: Int => A) = ()
    def sub[A](v: T[A], i:Int) = MatrixErrors.outOfBounds
    def map[A,B](v: T[A], f: (Int,A) => B) = ()
    def update[A,B>:A](v: T[A], i: Int, x: B) = ()
  }

  val opsI = new Ops[Id] {
    val size = 1
    def apply[A](f: Int => A) = f(0)
    def sub[A](v: T[A], i:Int) =
      if(i == 0) v else MatrixErrors.outOfBounds
    def map[A,B](v: T[A], f: (Int,A) => B) = f(0,v)
    def update[A,B>:A](v: T[A], i: Int, x: B) = sub(x,i)
  }

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


/*
 *fastexp E I 7 =
 * fastexp EI II 3 =
 * fastexp (EI)(II) (II)(II) 1 =
 * fastexp ((EI)(II))((II)(II)) _ 0
 *
  *
  * 
  * fastexp E I 6 =
  * fastexp E II 3 =
  * fastexp (E)(II) (II)(II) 1 =
  * fastexp ((E)(II))((II)(II))  __ 0
  /* Sized vector subscript functions. */
  case class subP[V[_],W[_]](subv: Getter[V], subw: Getter[W], vsize: Int)
  extends Getter[Pr[V,W]#T] {
    def apply[A](v: Pair[V,W,A], i: Int): A =
      if(i < vsize) subv(v._1, i)
      else subw(v._2, i-vsize)
    def map[A,B](v: Pair[V,W,A], f: (Int,A) => B) =
      (subv.map(v._1, f), subw.map(v._2, {(i,a:A) => f(i+vsize, a)}))
  }

  /* For convenience and testing, here are some specializations for
     small lengths. */
  trait VectorSpec {
//    type V[_]
//    type W[_]
//    type T[A] = Pair[V,W,A]
    type T[_]
    def size: Int
    def apply[A](f: Int => A): T[A]
    def sub[A](v: T[A], i: Int): A
//    def map[A,B](v: T[A], f: A => B): T[B]
  }

  object One extends VectorSpec {
    /* fastexp E I 1 =                 ODD
     * fastexp Pr[E,I] Pr[I,I] 0 =     ZERO
     * Pr[E,I]
     * */
    type T[A] = Pair[Empty,Id,A]
    def apply[A](f: Int => A): T[A] = mkP(mkE,mkI,0)(f)
    def sub[A](v: T[A], i: Int): A = subP(subE, subI, 0)(v,i)
    def size = 1
  }
  object Two extends VectorSpec {
    /* fastexp E I 2 =                EVEN
     * fastexp E Pr[I,I] 1 =          ODD
     * fastexp Pr[E,Pr[I,I]] _ 0 =    ZERO
     * Pr[E,Pr[I,I]]
     */
    type T[A] = Pair[Empty,Pr[Id,Id]#T,A]
    def apply[A](f: Int => A): T[A] =
      mkP[Empty,Pr[Id,Id]#T](mkE,mkP(mkI,mkI,1),0)(f)
    def sub[A](v: T[A], i: Int): A =
      subP[Empty,Pr[Id,Id]#T](subE, subP(subI, subI, 1), 0)(v,i)
    def size = 2
  }
  object Three extends VectorSpec {
    /* fastexp E I 3 =                      ODD
     * fastexp Pr[E,I] Pr[I,I] 1 =          ODD
     * fastexp Pr[Pr[E,I],Pr[I,I]] _ 0 =    ZERO
     * Pr[Pr[E,I],Pr[I,I]]
     */
    type T[A] = Pair[Pr[Empty,Id]#T, Pr[Id,Id]#T, A]
    def apply[A](f: Int => A): T[A] =
      mkP[Pr[Empty,Id]#T, Pr[Id,Id]#T](
        mkP(mkE,mkI,0),
        mkP(mkI,mkI,1),
        1)(f)
    def sub[A](v: T[A], i: Int): A =
      subP[Pr[Empty,Id]#T, Pr[Id,Id]#T](
        subP(subE,subI,0),
        subP(subI,subI,1),
        1)(v,i)
    def size = 3
  }
  object Four extends VectorSpec {
    /* fastexp E I 4 =                            EVEN
     * fastexp E Pr[I,I] 2 =                      EVEN
     * fastexp E Pr[Pr[I,I],Pr[I,I]] 1 =          ODD
     * fastexp Pr[E,Pr[Pr[I,I],Pr[I,I]]] _ 0 =    ZERO
     * Pr[E,Pr[Pr[I,I],Pr[I,I]]]
     */
     *
  * fastexp E I 5 =
  * fastexp EI II 2 =
  * fastexp EI (II)(II) 1 =
  * fastexp (EI)((II)(II)) __ 0
    type T[A] = Pair[Empty, Pr[Pr[Id,Id]#T, Pr[Id,Id]#T]#T, A]
    def apply[A](f: Int => A): T[A] =
      mkP[Empty, Pr[Pr[Id,Id]#T, Pr[Id,Id]#T]#T](
        mkE,
        mkP[Pr[Id,Id]#T, Pr[Id,Id]#T](
          mkP(mkI,mkI,1),
            mkP(mkI,mkI,1),
          2),
        0)(f)
    def sub[A](v: T[A], i: Int): A =
      subP[Empty, Pr[Pr[Id,Id]#T, Pr[Id,Id]#T]#T](
        subE,
        subP[Pr[Id,Id]#T, Pr[Id,Id]#T](
          subP(subI, subI, 1),
          subP(subI, subI, 1),
          2),
        0)(v,i)
    def size = 4
  }
  */
}

/*
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
      subv(subv(data, i), j)
    override def toString: String = "Zero " + data.toString
  }

  /*private*/ case class Odd[V[_],W[_],A]
      (next: FE_M[Pr[V,W]#T, Pr[W,W]#T, A])
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
      (next: FE_M[V, Pr[W,W]#T, A])
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
      Odd[Empty,Id,A](Zero[Pr[Empty,Id]#T, Pr[Id,Id]#T, A](
        One(i => One.apply[A](j => f(i,j))))))

  def tabulate[A](n: Int)(f:(Int,Int) => A): M[A] = {
    def loop[V[_],W[_]](mkv: Maker[V], mkw: Maker[W],
                        vsize: Int, wsize: Int, k: Int): FE_M[V,W,A] =
      if(k == 0)
        Zero(mkv(i => mkv(j => f(i,j))))
      else if(k%2 == 1)
        Odd(loop[Pr[V,W]#T, Pr[W,W]#T](
          mkP(mkv,mkw,vsize), mkP(mkw,mkw,wsize),
          vsize+wsize, wsize*2, k/2))
      else
        Even(loop[V, Pr[W,W]#T](
          mkv, mkP(mkw,mkw,wsize),
          vsize, wsize*2, k/2))
    FE_Matrix(loop(mkE, mkI, 0, 1, n))
  }
}
*/
