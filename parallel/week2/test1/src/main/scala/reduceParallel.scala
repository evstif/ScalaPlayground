import java.util.concurrent.FutureTask

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import scala.concurrent._
import ExecutionContext.Implicits.global

object reduceParallel {
  val threshold = 5
  def reduce[A](list: List[A], start: Int, end: Int, f: (A, A) => A): A = {
    if (start - end < threshold){
      var res = list.head
      for (idx <- start until (end - 1)) {
        res = f(res, list(idx+1))
      }
      res
    } else {
      val mid = (start - end) / 2

      def producer() = {
        val results = Seq(
          Future { reduce(list, start, mid, f) },
          Future { reduce(list, mid, end, f) }
        )
        Future.sequence(results)
      }

      val res = Await.result(producer, Duration.Inf)
      f(res.head, res.tail.head)
    }
  }
}

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val list = 1 to 500 toList
    val sum = reduceParallel.reduce(list, 0, list.length, (a: Int, b: Int) => a + b)
    println(s"Reduced sum is $sum")
  }
}