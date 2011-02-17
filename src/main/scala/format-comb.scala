import java.io.PrintStream

/* Compose two type constructors. */
trait Compose[F[_],G[_]] { type T[X] = F[G[X]] }

object Format {
  /* Buffer representation; could be abstracted out. */
  type Buf = List[String]
  def put(b: Buf, e: String): Buf = e :: b
  def finish(b: Buf): String = b.reverse.mkString
  def initial: Buf = Nil

  /* The basic mechanism: composable fragments with an abstract type
     constructors that can add parameters. */
  type Cont[A] = Buf => A
  trait Fragment[F[_]] {
    def apply[A](k: Cont[A]): Cont[F[A]]

    def & [G[_]] (g: Fragment[G]) = new Fragment[Compose[F,G]#T] {
      def apply[A](k: Cont[A]) =
        Fragment.this(g(k))
    }

    def | : F[String] = apply(finish(_))(initial)
    def |> (ps: PrintStream): F[Unit] =
      apply{b:Buf => ps.print(finish(b))}(initial)
    def |> : F[Unit] = |> (Console.out)
  }

  /* Combinators that don't add parameters are called "glue". */
  type Id[A] = A
  case class Glue(s: String) extends Fragment[Id] {
    def apply[A](k: Cont[A]): Cont[A] = (b:Buf) => k(put(b,s))
  }

  implicit def literalString(s:String) = Glue(s)
  val endl = Glue("\n")

  /* as presented in talk: */
  /*implicit*/ def lit(s:String) = new Fragment[Id] {
    def apply[A](k: Cont[A]) = (b:Buf) => k(put(b,s))
  }
  val xx = new Fragment[IntF] {
    def apply[A](k: Cont[A]) = (b:Buf) => (i:Int) => k(put(b,i.toHexString))
  }

  /* Combinators for base types. */

  trait F1[A] { type T[R] = A => R }

  def preprocess[A](f: A => String) = new Fragment[F1[A]#T] {
      def apply[R](k: Cont[R]) =
        (b: Buf) => (a: A) => k(put(b, f(a)))
  }

  type AnyF[R] = Any => R
  type IntF[R] = Int => R
  type FloatF[R] = Float => R
  type DoubleF[R] = Double => R
  type NumberF[R] = Number => R

  val s: Fragment[AnyF] = preprocess(_.toString)
  val d: Fragment[IntF] = preprocess(_.toString)
  val x: Fragment[IntF] = preprocess(_.toHexString)
  val f: Fragment[FloatF] = preprocess(_.toString)
  val lf: Fragment[DoubleF] = preprocess(_.toString)

  import java.text.NumberFormat
  val n: Fragment[NumberF] = preprocess(NumberFormat.getInstance.format(_))
  val i: Fragment[IntF] = preprocess(NumberFormat.getIntegerInstance.format(_))
  val pct: Fragment[DoubleF] =
    preprocess(NumberFormat.getPercentInstance.format(_))
  val cur: Fragment[DoubleF] =
    preprocess(NumberFormat.getCurrencyInstance.format(_))

  import java.util.Formatter
  val juf = new Formatter
  def f(prec: Int): Fragment[FloatF] = {
    val fms = "%." & i & "f" | prec
    def fmt(x:Float) =
      juf.format(fms, x.asInstanceOf[AnyRef]).toString
    preprocess(fmt)
  }
}

object FormatExamples extends Application {
  import Format._
  import Function.uncurried

  val a = 5
  val b = 2
  val c = a / b.toFloat

  val frac: Int => Int => Float => String =
    d & " over " & d & " is " & f(2) & endl |

  val grade: Any => Double => Unit =
    "Hello, "& s& ": your exam score is "& pct& endl |>

  val hex: (Int, Int, Int) => String = uncurried("#"&x&x&x|)

  println(uncurried(frac)(a,b,c))
  grade("Joshua")(0.97)
  println("Roses are "&s | hex(250, 21, 42))

  val toHex = uncurried("#" & x & x & x |)
  val greeting = "Hello, " & s & endl |>

  greeting("Chris")
  greeting("NEScala")

//  "Roses are " & s & n & s & n |> () toHex(255,24,37) apply (new Date)
}
