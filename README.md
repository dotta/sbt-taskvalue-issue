sbt: taskValue & concurrent restrictions
========================================

This repository serves the purpose of showcasing an issue I'm hitting when mixing `taskValue` and settings some tag to it. The reason why I'm doing so is that I would like to override the implementation of `compileIncremental` to decorate it and assign a tag that is dynamically computed based on the number of sources that are compiled. The end goal is to limit parallel execution for the `compileIncremental` task with the help of a custom Tag.

MyPlugin
--------

### Problem

The `project/src/main/scala/MyPlugin.scala` exemplifies the sbt plugin I've created to reach the above described goal. As it can be observed by compiling this project, the current implementation doesn't work, since the modules `a` and `b` are compiled in parallel. The `compileIncremental` task has been overridden as explained in https://www.scala-sbt.org/1.x/docs/Tasks.html#Modifying+an+Existing+Task, but it seems that `MyTag` is essentially dropped.

To workaround the problem, I also tried using `.taskValue`, but I've hit the exact same problem and I'm stuck at this point. In fact, the only workaround I found is to call explicitly `Defaults.compileIncrementalTask`, but I'd really like to avoid.

By the way, it's worth noticing that if `Tags.CPU` is set to `1` for the `concurrentRestrictions`, then compilation is carried out sequentially. This highlights that the original tags set in the sbt implementation of `compileIncremental` are kept, and it's just the additional `MyTag` that is missing.

Question
--------

Is there a workaround to achieved the desired sequential compilation without explicitly delegating to `Defaults.compileIncrementalTask`?

Also, note that for my use-case I'd really need a solution that works with `Def.taskDyn` and leverages `.taskValue`, a the tag I need to associate to `compileIncremental` is computed dynamically based on the number of sources that are being compiled.

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
