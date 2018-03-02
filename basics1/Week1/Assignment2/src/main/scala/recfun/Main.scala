package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (r == 0) || (r == c)) 1
    else if ((c < 0) || (c > r)) 0
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val res = chars.foldLeft(0)((acc: Int, cur: Char) => {
      if (acc < 0)
        acc
      else if (cur == '(')
        acc + 1
      else if (cur == ')')
        acc - 1
      else
        acc
    })
    res match {
      case 0 => true
      case _ => false
    }
  }

  /**
    * Exercise 3
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
}
