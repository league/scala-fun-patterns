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

object QuadTreeSpec
extends SquareMatrixSpec(QuadTreeTestParams, "QuadTreeSpec")


/*
object FastExpTestParams extends SquareMatrixTestParams {
  val factory = FastExpSquareMatrix
  val dimension = Gen.choose(0,40)
}
object FastExpSpec
extends SquareMatrixSpec(FastExpTestParams, "FastExpSpec")
abstract class SizedVectorSpec(sv: SizedVectors.VectorSpec,
                               nm: String) extends Properties(nm) {
  def frob(x: Int) = x * (x+1)
  private val sample = sv(frob)

  def borf(i: Int, x: Int) = i + x*x
  val coord = Gen.choose(0,sv.size-1)

  property("elements") = Prop.forAll(coord) {
    i => sv.sub(sample,i) == frob(i)
  }
}

object VectorOneSpec
extends SizedVectorSpec(SizedVectors.One, "one")

object VectorTwoSpec
extends SizedVectorSpec(SizedVectors.Two, "two")

object VectorThreeSpec
extends SizedVectorSpec(SizedVectors.Three, "three")

object VectorFourSpec
extends SizedVectorSpec(SizedVectors.Four, "four")
*/
