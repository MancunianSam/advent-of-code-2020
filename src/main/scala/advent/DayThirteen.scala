package advent

import scala.io.Source

object DayThirteen extends App {

  val lines = Source.fromResource("13").getLines.toList
  val timeToLeave = lines.head.toInt
  val timesWithIdx = lines.last.split(",").zipWithIndex.map {
    case (time, idx) => (time, idx)
  }.filter(p => p._1 != "x")
    .map(f => (f._1.toLong,f._2))

    val busTimes = lines.last.split(",").filter(_ != "x").map(_.toInt)
    val nearestTimes = busTimes.map(time => {
      val nearestTime = time * (timeToLeave / time + 1)
      (time, nearestTime - timeToLeave)
    }).minBy(f => f._2)

  val idxToTotalIncrement = for {
    (k,v) <- timesWithIdx.tail.scan(timesWithIdx.head)((a, b) =>
      (a._1 * b._1, b._2)
    ).toMap
  } yield (v, k)

  val timesWithIdxAndIncrement = timesWithIdx.map(f => (f._1, f._2, idxToTotalIncrement(f._2)))

  val (bus, idx, increment) = timesWithIdxAndIncrement.head
  val result: (Long, Int, Long, List[Long]) = timesWithIdxAndIncrement.tail.foldLeft((bus, idx, increment, List(0L)))((a, b) => {
    def count(total: Long): Long = {
      if((total + b._2) % b._1== 0) {
        total
      } else {
        count(total + a._3)
      }
    }
    val totalCount = count(a._4.head)
    (b._2, b._2, b._3, totalCount :: a._4)
  })

  println(nearestTimes._1 * nearestTimes._2)
  println(result._4.head)
}
