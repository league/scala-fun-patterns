import java.io.PrintStream

/* Compose two type constructors. */
trait Compose[F[_],G[_]] { type T[X] = F[G[X]] }

object Format {
  /* Buffer representation; could be abstracted out. */
  type Elem = String
  type Buf = List[Elem]
  type Result = String
  def put(b: Buf, e: Elem): Buf = e :: b
  def finish(b: Buf): Result = b.reverse.mkString
  def initial: Buf = Nil

  /* The basic mechanism: composable fragments with an abstract type
     constructors that can add parameters. */
  type Cont[R] = Buf => R
  trait Fragment[F[_]] {
    def run[R](k: Cont[R]): Cont[F[R]]

    def & [G[_]] (g: Fragment[G]) = new Fragment[Compose[F,G]#T] {
      def run[R](k: Cont[R]) =
        Fragment.this.run(g.run(k))
    }

    def | : F[Result] = run(finish(_))(initial)
    def |> (ps: PrintStream): F[Unit] =
      run{b:Buf => ps.print(finish(b))}(initial)
    def |> : F[Unit] = |> (Console.out)
  }

  /* Combinators that don't add parameters are called "glue". */
  type Id[R] = R
  case class Glue(s: String) extends Fragment[Id] {
    def run[R](k: Cont[R]): Cont[R] = (b:Buf) => k(put(b,s))
  }

  implicit def literalString(s:String) = Glue(s)
  val endl = Glue("\n")

  /* Combinators for base types. */

  trait F1[A] { type T[R] = A => R }

  def preprocess[A](f: A => String) = new Fragment[F1[A]#T] {
      def run[R](k: Cont[R]) =
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

  trait Pr[A,B,F[_],G[_]] { type T[R] = (F[A],G[B]) => R }
  def pr[A,B,F[_],G[_]](f: Fragment[F], g: Fragment[G]) =
    new Fragment[Pr[A,B,F,G]#T] {
      def run[R](k: Cont[R]) = {
        (b: Buf) => (p: (A,B)) =>
          val s1 = f|p._1
          val s2 = g|p._2
          k(put(put(b,s1),s2))
      }
    }
}

object FormatExamples extends Application {
  import Format._
  import Function.uncurried

  val toHex = uncurried("#" & x & x & x |)
  val greeting = "Hello, " & s & endl |>

  greeting("Chris")
  greeting("NEScala")

//  "Roses are " & s & n & s & n |> () toHex(255,24,37) apply (new Date)
}
