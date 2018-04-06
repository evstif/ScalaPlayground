package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeInt(m: Int, c: List[Int]): Int = {
      if (m == 0)
        1
      else if (m < 0 || c.isEmpty)
        0
      else
        countChangeInt(m - c.head, c) + countChangeInt(m, c.tail)
    }

    countChangeInt(money, coins)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    def countChangeInt(m: Int, c: List[Int]): Int = {
      if (m == 0)
        1
      else if (m < 0 || c.isEmpty)
        0
      else if (threshold(m, c))
        countChangeInt(m - c.head, c) + countChangeInt(m, c.tail)
      else {
        val (r1, r2) = parallel(countChangeInt(m - c.head, c), countChangeInt(m, c.tail))
        r1 + r2
      }
    }

    countChangeInt(money, coins)
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    (m: Int, c: List[Int]) => {
      partialAmountThreshold(startingMoney, m)
    }
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    (m: Int, c: List[Int]) => {
      partialAmountThreshold(totalCoins, c.length)
    }
  }

  def partialAmountThreshold(base: Int, value: Int) : Boolean = {
    if ((2*base)/3 < value)
      true
    else
      false
  }

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (m: Int, c: List[Int]) => {
      m * c.length <= (startingMoney * allCoins.length)/2
    }
  }
}
