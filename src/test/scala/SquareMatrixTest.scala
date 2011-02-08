import org.scalacheck._
import org.scalacheck.Prop._

object QuadTreeCreateSpec extends Properties("quad tree create") {

  type M[A] = QuadTreeSquareMatrix[A]
  val factory = QuadTreeSquareMatrix

  val powerOfTwo = Gen.choose(0,10).map(1 << _)
  val fillMatrix = for { n <- powerOfTwo ; c <- Gen.alphaChar }
                   yield factory.fill(n)(c)
  val pairMatrix = powerOfTwo.map(factory.tabulate(_){(i,j)=>(i,j)})
  def coord[A](m: M[A]) = for {i <- Gen.choose(0, m.height-1);
                               j <- Gen.choose(0, m.width-1)}
                          yield (i,j)

  property("bounds") = Prop.forAll(fillMatrix) {
    m:M[Char] => Prop.forAll(coord(m)) {
      ij => m(ij) == m(0,0)
    }
  }

  property("coords") = Prop.forAll(pairMatrix) {
    m => Prop.forAll(coord(m)) {
      ij => m(ij) == ij
    }
  }
}
