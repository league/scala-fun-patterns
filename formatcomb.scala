type Buffer = List[String]
type Format[A] = Buffer => A

trait Compose[F[_],G[_]] {
  type T[X] = F[G[X]]
}

trait Fragment[F[_]] {
  def apply[A](k: Format[A]): Format[F[A]]
  def %[G[_]](g: Fragment[G]): Fragment[Compose[F,G]#T] = {
    new Fragment[Compose[F,G]#T] {
      def apply[A](k: Format[A]) = Fragment.this(g(k))
    }
  }
}

type IntF[A] = Int => A
val int = new Fragment[IntF] {
  def apply[A](k: Format[A]) = (b:Buffer) => (i:Int) => k((i.toString)::b)
}

type Id[A] = A
implicit def lit(s:String) = new Fragment[Id] {
  def apply[A](k: Format[A]) = (b:Buffer) => k(s::b)
}

def format[F[_]](k: Fragment[F]): F[Buffer] =
  k {b => b.reverse.mkString} (Nil)



// Polymorphic function values (see Rúnar, Higher-Rank Polymorphism in Scala)
trait ~> [F[_],G[_]] {
  def apply[A](x: F[A]): G[A]
  def compose[E[X],F2[X] <: F[X]](e: E ~> F2) = {
    val f = this
    new (E ~> G) { def apply[A](x: E[A]) = f(e(x)) }
  }
}

/* not sure compose is general enough. Need a test case where
   the 'F's don't quite match up.

   When I compose polymorphic functions,
     f: ∀α. F[α] → G[α]
     e: ∀β. E[β] → F’[β]
     f ○ e: ∀γ.   where 
   So here's the issue:
   *
  *
  *
  * 
*/
// TRYING AGAIN
trait ~> [F[_], G[_]] {
  def apply[A](x: F[A]): G[A]
  def compose[E[_], FA[_], F0[X] <: F[FA[X]], G0[X] >: G[FA[X]]] (e: E ~> F0) = {
    val f = this
    new (E ~> G0) { def apply[A](x: E[A]) = f(e(x)) }
  }
}



type Buffer = List[String]
type Format[A] = Buffer => A
type Glue = Format ~> Format

implicit def literalString(s: String): Glue =
  new Glue {
    def apply[A](k: Format[A]) = (b:Buffer) => k(s::b)
  }

type StringF[A] = Format[String => A]
val str = new (Format ~> StringF) {
  def apply[A](k: Format[A]): StringF[A] = (b:Buffer) => (s:String) => k(s::b)
}


object FormatComb1 {
  // This version requires too many type annotations when composing.
  def eol[R](k: String=>R)(s: String) = k(s+"\n")
  def int[R](k: String=>R)(s: String)(i: Int) = k (s + i.toString)
  def %[A,B,C](f: B=>C, g: A=>B)(a:A) = f(g(a))
  val x = 5
  def main(args: Array[String]) {
    println("Hello")
  }
}

object FormatComb2 {
  trait ~> [F[_],G[_]] {
    def apply[A](a: F[A]): G[A]
    def compose[H[_]](g: H ~> F) = {
      val f = this
      new (H ~> G) {
        def apply[A](a: H[A]): G[A] = f(g(a))
      }
    }
  }
  type Id[A] = A
  val singletonList = new (Id ~> List) {
    def apply[A](a:A): List[A] = List(a)
  }
  def doIt[B](f: Id ~> List, b: B, s: String): (List[B], List[String]) =
    (f(b), f(s))

  type TwiceArg[A] = (A, A => A)
  val twice = new (TwiceArg ~> Id) {
    def apply[A](a:TwiceArg[A]): A = a._2(a._2(a._1))
  }
  val head = new (List ~> Id) {
    def apply[A](a:List[A]): A = a.head
  }
  val tail = new (List ~> List) {
    def apply[A](a:List[A]): List[A] = a.tail
  }
}

trait Fragment[F[_]] {
  println("Creating "+this)
  def reallyApply[A](k: String=>A): String=>F[A]
  def apply[A](k: String=>A): String=>F[A] = {
    println("Applying "+this)
    reallyApply(k)
  }
  def % [G[_]] (g: Fragment[G]) = {
    type FG[A] = F[G[A]]
    val f = this
    new Fragment[FG] {
      println("Making combined "+this)
      def reallyApply[A](k: String=>A): String=>FG[A] = {
        f[G[A]](g[A](k))
      }
    }
  }
}

type Id[A] = A
val eol = new Fragment[Id] {
  def reallyApply[A](k: String=>A) = (s: String) => k(s+"\n")
}
type IntArg[A] = Int => A
val int = new Fragment[IntArg] {
  def reallyApply[A](k: String=>A) = (s: String) => (i: Int) => k(s+(i.toString))
}
type CharArg[A] = Char => A
val ch = new Fragment[CharArg] {
  def reallyApply[A](k: String=>A) = (s: String) => (c: Char) => k(s+(c.toString))
}
