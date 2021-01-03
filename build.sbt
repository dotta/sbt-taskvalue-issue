name := "issue-taskvalue"

def commonSettings = Seq(
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
)

lazy val a = (project in file("a")).settings(commonSettings)
lazy val b = (project in file("b")).settings(commonSettings)