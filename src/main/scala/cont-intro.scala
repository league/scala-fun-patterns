object ContinuationPassing extends Application {

  def greeting [A] (name: String) (k: => A): A =
    printk("Hello, ") {
      printk(name) {
        printk("!\n")(k)
      }
    } /* hello */

  def printk [A] (s: String) (k: => A): A =
    { Console.print(s); k }


  def plus [A] (x: Int, y: Int) (k: Int => A): A =
    k(x+y)

  def times [A] (x: Int, y: Int) (k: Int => A): A =
    k(x*y)

  def less [A] (x: Int, y: Int) (kt: => A) (kf: => A): A =
    if(x < y) kt else kf

  def test[A](k: String => A): A =
    plus(3,2) { a => times(3,2) { b =>
    less(a,b) {k("yes")} {k("no")} }}
}
