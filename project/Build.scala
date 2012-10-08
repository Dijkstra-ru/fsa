import sbt._
import ProguardPlugin._


object B extends Build {
	lazy val proguard = proguardSettings ++ Seq(
		proguardOptions := Seq(keepMain("ru.dijkstra.fsa.TestParser"))
	)

  lazy val B = Project("FEA", file(".")).settings(proguard: _*)
}