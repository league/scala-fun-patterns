type Quad[A] = (A,A,A,A)

object TopDown {
  abstract class Square[A]
  case class Zero[A](value: A) extends Square[A]
  case class Succ[A](value: Quad[Square[A]]) extends Square[A]
}
object BottomUp {
  abstract class Square[A] {
    val base: Int
    val dim: Int
  }
  case class Zero[A](value: A) extends Square[A] {
    val base = 0
    val dim = 1
  }
  case class Succ[A](value: Square[Quad[A]]) extends Square[A] {
    val base = value.base + 1
    val dim = value.dim * 2
  }
  val m1 = Zero(3)
  val m2 = Succ(Zero((4,5,6,7)))
  val m3 = Succ(Succ(Zero(((1,2,3,4),(2,3,4,5),(3,4,5,6),(4,5,6,7)))))

  def create[A](n: Int, x: => A): Square[A] = n match {
    case 0 => Zero(x)
    case _ => Succ(create(n-1, (x,x,x,x)))
  }

  def tabulate[A](n: Int, f: (Int, Int) => A): Square[A] =
    if(n == 0) Zero(f(0,0))
    else Succ(tabulate(n-1, {(i,j) => (f(i*2, j*2),
                                       f(i*2, j*2+1),
                                       f(i*2+1, j*2),
                                       f(i*2+1, j*2+1))}))
  def index[A,R](m: Square[A], i: Int, j: Int, k: A => R): R =
    m match {
      case Zero(data) => if(i == 0 && j == 0) k(data)
                         else error("out of bounds")
      case Succ(subm) => index(subm, i % subm.dim, j % subm.dim,
                               (q:Quad[A]) => (i / subm.dim, j / subm.dim) match {
                                 case (0,0) => k(q._1)
                                 case (0,1) => k(q._2)
                                 case (1,0) => k(q._3)
                                 case (1,1) => k(q._4)
                                 case _ => error("out of bounds")
                               })
    }
}

