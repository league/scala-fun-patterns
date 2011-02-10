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
      def f(i:Int, j: Int, xy: (Int,Int)) =
        i == xy._1 && j == xy._2
      val m2 = m.map(f)
      Prop.forAll(coord(m2)) {
        ij => m2(ij)
      }
  }

}

object QuadTreeTestParams extends SquareMatrixTestParams {
  val factory = QuadTreeSquareMatrix
  val dimension = Gen.choose(0,6).map(1 << _) // powers of two
}

object QuadTreeSpec
extends SquareMatrixSpec(QuadTreeTestParams, "QuadTreeSpec")

class SizedVectorSpec[V[+_]](ops: SizedVectors.Ops[V], nm: String)
extends Properties(nm) {
  def frob(x: Int) = (x+1) * (x+2)
  private val sample1 = ops(frob)

  def borf(i: Int, x: Int) = i + x*x
  private val sample2 = ops.map(sample1, borf)

  val coord = Gen.choose(0, ops.size-1)

  property("elements") = Prop.forAll(coord) {
    i => ops.sub(sample1, i) == frob(i)
  }
  property("map") = Prop.forAll(coord) {
    i => ops.sub(sample2, i) == borf(i, frob(i))
  }
  property("update") = Prop.forAll(coord) {
    i => val sample3 = ops.update(sample2, i, 9999)
      ops.sub(sample3, i) == 9999 &&
      (ops.size < 2 ||
       Prop.forAll(coord) {
         j => j != i ==> (ops.sub(sample3,j) == ops.sub(sample2,j))
       })
  }
}

object Vector1Spec
extends SizedVectorSpec[SizedVectors.ops1.T](SizedVectors.ops1, "1")

object Vector2Spec
extends SizedVectorSpec[SizedVectors.ops2.T](SizedVectors.ops2, "2")

object Vector3Spec
extends SizedVectorSpec[SizedVectors.ops3.T](SizedVectors.ops3, "3")

object Vector4Spec
extends SizedVectorSpec[SizedVectors.ops4.T](SizedVectors.ops4, "4")

object Vector5Spec
extends SizedVectorSpec[SizedVectors.ops5.T](SizedVectors.ops5, "5")

object Vector6Spec
extends SizedVectorSpec[SizedVectors.ops6.T](SizedVectors.ops6, "6")

object FastExpTestParams extends SquareMatrixTestParams {
  val factory = FastExpSquareMatrix
  val dimension = Gen.choose(0,40)
}

object FastExpSpec
extends SquareMatrixSpec(FastExpTestParams, "FastExpSpec")
