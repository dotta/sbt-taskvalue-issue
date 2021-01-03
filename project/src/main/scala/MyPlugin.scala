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
      concurrentRestrictions += Tags.limit(MyTag, 1)
  )

  override def projectSettings =
    inConfig(Compile)(compileSettings) ++ inConfig(Test)(compileSettings)

  private def compileSettings: Seq[Def.Setting[_]] = Seq(
    ///*
    // THIS DOESN'T WORKS: Concurrent restrictions for MyTag are NOT taken into account.
    compileIncremental := Def.taskDyn {
        val compileIncrementalTask = compileIncremental.taskValue
        Def.task(compileIncrementalTask.tag(MyTag).value)
    }.value
    //*/
    /*
    // THIS WORKS: Concurrent restrictions for MyTag are correctly taken into account
    // However, `Defaults.compileIncrementalTask` is explicitly called, which is not ideal.
    // Ideally, we would like to call `compileIncremental`, but doing so here in place of
    // `Defaults.compileIncrementalTask` results in a deadlock when executing `;clean;compile`.
    // (Hence, the behavior described in
    // https://www.scala-sbt.org/1.x/docs/Tasks.html#Modifying+an+Existing+Task doesn't work
    // when a task is tagged - which seems a distinct problem from the one due to the usage of
    // taskValue)
    compileIncremental := Def.taskDyn {
        Def.task(Defaults.compileIncrementalTask.tag(MyTag).value)
    }.value
    */
    /*
    // This deadlocks when calling ';clean;compile'
    compileIncremental := Def.taskDyn {
        Def.task(compileIncremental.tag(MyTag).value)
    }.value
    */
  )

}
