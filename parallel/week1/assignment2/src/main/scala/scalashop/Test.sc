val list = (1 to 31).toVector

val numTasks = 3

val colsPerTaks = Math.max(list.length / numTasks,1)
val startPoints = Range(0, list.length) by colsPerTaks

val tasks = startPoints.map(t => {
  val step = t + Math.max(list.length / numTasks,1)
  println(s"1: $t, 2: $step")
  1
})
