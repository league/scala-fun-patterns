import java.io.PrintStream
import java.util.Date

object FormatCombinators {
  type B = List[String]
  type Format[A] = B => A
  trait Compose[F[_],G[_]] { type T[X] = F[G[X]] }
  trait Fragment[F[_]] {
    def apply[A](k: Format[A]): Format[F[A]]
    def % [G[_]] (g: Fragment[G]) =
      new Fragment[Compose[F,G]#T] {
        def apply[A](k: Format[A]) = Fragment.this(g(k))
      }
    def format[A](k: String => A): F[A] =
      this{b:B => k(b.reverse.mkString)}(Nil)
    def sprintf : F[String] = format{s=>s}
    def fprintf(out: PrintStream): F[Unit] = format{out.print(_)}
    def printf: F[Unit] = format{Console.out.print(_)}
  }

  type Id[A] = A
  implicit def literal(s:String) = new Fragment[Id] {
    def apply[A](k: Format[A]) = (b:B) => k(s::b)
  }

  val n = new Fragment[Id] {
    def apply[A](k: Format[A]) = (b:B) => k("\n"::b)
  }

  type AnyF[A] = Any => A
  val s = new Fragment[AnyF] {
    def apply[A](k: Format[A]) =
      (b:B) => (x:Any) => k(x.toString :: b)
  }

  type IntF[A] = Int => A
  val x: Fragment[IntF] = new Fragment[IntF] {
    def apply[A](k: Format[A]) =
      (b:B) => (n:Int) => k(n.toHexString :: b)
  }
}

object FormatExamples extends Application {
  import FormatCombinators._
  import Function.uncurried

  val toHex = uncurried("#" % x % x % x sprintf)
  val greeting = "Hello, " % s % n printf

  greeting("Chris")
  greeting("NEScala")

  "Roses are " % s % n % s % n printf toHex(255,24,37) apply (new Date)
}
