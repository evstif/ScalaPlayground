package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    balanceCount(chars, 0, chars.length) match {
      case 0 => true
      case _ => false
    }
  }

  def balanceCount(chars: Array[Char], s: Int, e: Int) : Int = {
    chars.slice(s, e).foldLeft(0)((acc: Int, cur: Char) => {
      if (acc < 0)
        acc
      else if (cur == '(')
        acc + 1
      else if (cur == ')')
        acc - 1
      else
        acc
    })
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

//    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
//      ???
//    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from < threshold)
        balanceCount(chars, from, until)
      else {
        val mid = (until - from) / 2
        val (r1, r2) = parallel(balanceCount(chars, from, mid), balanceCount(chars, mid, until))
        r1 + r2
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
