import sbt._

class FunPatternsProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaCheck = ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8")
}
