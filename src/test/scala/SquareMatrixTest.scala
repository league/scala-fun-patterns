import org.scalacheck._
import org.scalacheck.Prop._

object QuadTreeCreateSpec extends Properties("quad tree create") {

  type M[A] = QuadTreeSquareMatrix[A]
  val factory = QuadTreeSquareMatrix

  val powerOfTwo = Gen.choose(0,6).map(1 << _)
  val fillMatrix = for { n <- powerOfTwo ; c <- Gen.alphaChar }
                   yield factory.fill(n)(c)
  def pairs(n: Int) = factory.tabulate(n){(i,j)=>(i,j)}
  val pairMatrix = powerOfTwo.map(pairs)
  def coord[A](m: M[A]) = for {i <- Gen.choose(0, m.height-1);
                               j <- Gen.choose(0, m.width-1)}
                          yield (i,j)

  property("bounds") = Prop.forAll(fillMatrix) {
    m:M[Char] => Prop.forAll(coord(m)) {
      ij =>
        ij._1 >= 0 && ij._2 >= 0 ==> // mysteriously, sometimes gets -1
          (m(ij) == m(0,0))
    }
  }

  property("coords") = Prop.forAll(pairMatrix) {
    m => Prop.forAll(coord(m)) {
      ij =>
        ij._1 >= 0 && ij._2 >= 0 ==> // mysteriously, sometimes gets -1
          (m(ij) == ij)
    }
  }

  property("update") = Prop.forAll(fillMatrix) {
    m => Prop.forAll(coord(m)) {
      ij =>
        val m2 = m.updated(ij._1, ij._2, '!')
        m2(ij) == '!' && Prop.forAll(coord(m)) {
          kl => ij != kl ==> (m2(kl) == m(kl))
        }
    }
  }

  property("map") = Prop.forAll(pairMatrix) {
    m =>
      def f(xy: (Int,Int)) = xy._1 - xy._1
      val m2 = m.map(f)
      Prop.forAll(coord(m2)) {
        ij => f(m(ij)) == m2(ij)
      }
  }
}
