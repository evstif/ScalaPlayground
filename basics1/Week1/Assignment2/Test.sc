def countChange(money: Int, coins: List[Int]): Int = {

  def countChangeInt(m: Int, c: List[Int]): Int = {
    if (m < 0 || c.isEmpty )
      0
    else if (m == 0 )
      1
    else
      countChangeInt(m - c.head, c) + countChangeInt(m, c.tail)
  }

  countChangeInt(money, coins)
}

countChange(4, List(1,2))