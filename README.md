sbt: taskValue & concurrent restrictions
========================================

This repository serves the purpose of showcasing an issue I'm hitting when mixing `taskValue` and settings some tag to it. The reason why I'm doing so is that I would like to override the implementation of `compileIncremental` to decorate it and assign a tag that is dynamically computed based on the number of sources that are compiled. The end goal is to limit parallel execution for the `compileIncremental` task with the help of a custom Tag.

MyPlugin
--------

### Problem 1: .taskValue

The `project/src/main/scala/MyPlugin.scala` exemplifies the sbt plugin I've created to reach the above described goal. As it can be observed by compiling this project, the current implementation doesn't work, since the modules `a` and `b` are compiled in parallel. This seems to hint that it's not possible to associate tags to the result of `.taskValue`. In fact, if the alternative implementation of `compileIncremental` in `MyPlugin.scala` is used (it is currently commented), then compilation happens sequentially as desired.

### Problem 2: deadlock when overriding a task

Interestingly, I noticed a second issue unrelated to `.taskValue` when overriding the `compileIncremental` task as described in https://www.scala-sbt.org/1.x/docs/Tasks.html#Modifying+an+Existing+Task. By associating a tag to it I managed to deadlock sbt when executing `;clean ;compile` on the root project.

Question
--------

Is there a workaround to achieved the desired sequential compilation without explicitly delegating to `Defaults.compileIncrementalTask`?

Additional info
---------------

```
$ sbt --launcher-version
sbt launcher version 1.1.6
```

```
$ javac -version
javac 1.8.0_202
```
