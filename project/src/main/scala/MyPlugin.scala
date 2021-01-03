package sbtmyplugin

import sbt._
import sbt.Keys._

object MyPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = allRequirements

  object autoImport {
      val MyTag = Tags.Tag("mytag")
  }
  import autoImport._

  override def globalSettings = Seq(
      concurrentRestrictions ++= Seq(
        Tags.limit(MyTag, 1)
        // Uncomment this and compilation is carried out sequentially
        /*, Tags.limit(Tags.CPU, 1)*/)
  )

  override def projectSettings =
    inConfig(Compile)(compileSettings) ++ inConfig(Test)(compileSettings)

  private def compileSettings: Seq[Def.Setting[_]] = Seq(
    compileIncremental := {
        compileIncremental.tag(MyTag).value
    }
    // Alternative implementation, exactly same problem
    /*
    compileIncremental := Def.taskDyn {
        val compileIncrementalTask = compileIncremental.taskValue
        Def.task(compileIncrementalTask.value).tag(MyTag)
    }.value
    */
    // Alternative implementation that WORKS (but it's not ideal as it introduces
    // coupling with the sbt implementation, i.e., Defaults.compileIncrementalTask)
    /*
    compileIncremental := {
        Defaults.compileIncrementalTask.tag(MyTag).value
    }
    */
  )
}
