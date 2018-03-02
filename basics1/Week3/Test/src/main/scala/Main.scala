object Main extends  App{
  override def main(args: Array[String]): Unit = {
    def set1 = new NonEmpty(10)
    def set2 = new NonEmpty(3)
    def set3 = set1 add 5
    def set4 = (set1 union (set3 union set2)) union set1

    println(set1)
    println(set2)
    println(set3)
    println(set4)
  }
}
