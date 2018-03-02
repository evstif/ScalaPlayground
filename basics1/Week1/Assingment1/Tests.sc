import scala.annotation.tailrec


def fact(x: Int): Int = {
  @tailrec
  def facInt(x: Int, acc: Int): Int = {
    if (x == 0)
      acc
    else
      facInt(x-1, x * acc)
  }
  facInt(x, 1)
}

fact(4)