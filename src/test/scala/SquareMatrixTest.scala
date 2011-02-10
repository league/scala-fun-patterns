import org.scalacheck._
import org.scalacheck.Prop._

trait SquareMatrixTestParams {
  type M[A] = factory.M[A]
  val factory: SquareMatrixFactory
  val dimension: Gen[Int]

  def filled[A](elem: Gen[A]): Gen[M[A]] =
    dimension.flatMap(n => elem.map(factory.fill(n)(_)))

  def pairs(dim: Gen[Int]): Gen[M[(Int,Int)]] =
    dim.flatMap(factory.tabulate(_)(Pair.apply _))

  def coord[A](m: Matrix[A]) =
    Gen.choose(0, m.height-1).map2(Gen.choose(0, m.width-1))(Pair.apply _)
}

class SquareMatrixSpec(pm: SquareMatrixTestParams, nm: String)
extends Properties(nm) {
  import pm._
  property("bounds") =
    Prop.forAll(filled(Gen.alphaChar)) { m: M[Char] =>
      Prop.forAll(coord(m)) { ij =>
        m(ij) == m(0,0)
      }
    }

  property("coords") =
    Prop.forAll(pairs(dimension)) { m =>
      Prop.forAll(coord(m)) { ij =>
//        ij._1 >= 0 && ij._2 >= 0 ==> // mysteriously, sometimes gets -1
        (m(ij) == ij)
      }
    }

  property("update") = Prop.forAll(filled(Gen.alphaChar)) {
    m => Prop.forAll(coord(m)) {
      ij =>
        val m2 = m.updated(ij._1, ij._2, '!')
        m2(ij) == '!' && Prop.forAll(coord(m)) {
          kl => ij != kl ==> (m2(kl) == m(kl))
        }
    }
  }

  property("map") = Prop.forAll(pairs(dimension)) {
    m =>
      def f(xy: (Int,Int)) = xy._1 - xy._1
      val m2 = m.map(f)
      Prop.forAll(coord(m2)) {
        ij => f(m(ij)) == m2(ij)
      }
  }

}

object QuadTreeTestParams extends SquareMatrixTestParams {
  val factory = QuadTreeSquareMatrix
  val dimension = Gen.choose(0,6).map(1 << _) // powers of two
}

object QuadTreeSpec extends SquareMatrixSpec(QuadTreeTestParams, "QuadTreeSpec")

abstract class SizedVectorSpec(nm: String) extends Properties(nm) {
  type T[_]
  def size: Int
  def mk[A](f: Int => A): T[A]
  def sub[A](i: Int, v:T[A]): A

  def frob(x: Int) = x * (x+1)
  val sample = mk(frob)

  property("elements") = Prop.forAll(Gen.choose(0,size-1)) {
    i => sub(i,sample) == frob(i)
  }
}

object SingletonVectorSpec extends SizedVectorSpec("singleton") {
  import SizedVectors._
  /* fastexp E I 1 = fastexp Tw[E,I] Tw[I,I] 0 = Tw[E,I] */
  type T[A] = Two[Empty,Id,A]
  def mk[A](f: Int => A): T[A] = mkP(mkE,mkI,0)(f)
  def sub[A](i: Int, v: T[A]): A = subP(subE, subI[A], 0)(i, v)
  def size = 1
}

object PairVectorSpec extends SizedVectorSpec("pair") {
  import SizedVectors._
  /* fastexp E I 2 =
   * fastexp E Tw[I,I] 1 =
   * fastexp Tw[E,Tw[I,I]] _ 0 =
   * Tw[E,Tw[I,I]]
   */
  type T[A] = Two[Empty,Tw[Id,Id]#T,A]
  def mk[A](f: Int => A): T[A] = mkP[Empty,Tw[Id,Id]#T](mkE,mkP(mkI,mkI,1),0)(f)
  def sub[A](i: Int, v: T[A]): A =
    subP[Empty,Tw[Id,Id]#T,A](subE, subP(subI[A], subI[A], 1), 0)(i,v)
  def size = 2
}

object TripleVectorSpec extends SizedVectorSpec("triple") {
  import SizedVectors._
  /* fastexp E I 3 =
   * fastexp Tw[E,I] Tw[I,I] 1 =
   * fastexp Tw[Tw[E,I],Tw[I,I]] _ 0 =
   * Tw[Tw[E,I],Tw[I,I]]
   */
  type T[A] = Two[Tw[Empty,Id]#T, Tw[Id,Id]#T, A]
  def mk[A](f: Int => A): T[A] =
    mkP[Tw[Empty,Id]#T, Tw[Id,Id]#T](
      mkP(mkE,mkI,0),
      mkP(mkI,mkI,1),
      1)(f)
  def sub[A](i: Int, v: T[A]): A =
    subP[Tw[Empty,Id]#T, Tw[Id,Id]#T, A](
      subP(subE,subI[A],0),
      subP(subI[A],subI[A],1),
      1)(i,v)
  def size = 3
}

object QuadVectorSpec extends SizedVectorSpec("quad") {
  import SizedVectors._
  /* fastexp E I 4 =
   * fastexp E Tw[I,I] 2 =
   * fastexp E Tw[Tw[I,I],Tw[I,I]] 1 =
   * fastexp Tw[E,Tw[Tw[I,I],Tw[I,I]]] _ 0 =
   * Tw[E,Tw[Tw[I,I],Tw[I,I]]]
   */
  type T[A] = Two[Empty, Tw[Tw[Id,Id]#T, Tw[Id,Id]#T]#T, A]
  def mk[A](f: Int => A): T[A] =
    mkP[Empty, Tw[Tw[Id,Id]#T, Tw[Id,Id]#T]#T](
      mkE,
      mkP[Tw[Id,Id]#T, Tw[Id,Id]#T](
        mkP(mkI,mkI,1),
        mkP(mkI,mkI,1),
        2),
      0)(f)
  def sub[A](i: Int, v: T[A]): A =
    subP[Empty, Tw[Tw[Id,Id]#T, Tw[Id,Id]#T]#T, A](
      subE,
      subP[Tw[Id,Id]#T, Tw[Id,Id]#T, A](
        subP(subI[A], subI[A], 1),
        subP(subI[A], subI[A], 1),
        2),
      0)(i,v)
  def size = 4
}
