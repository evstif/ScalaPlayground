def isort(list: List[Int]): List[Int] = list match {
  case Nil => List()
  case h::t => insert(h, isort(t))
}

def insert(i: Int, ints: List[Int]): List[Int] = ints match {
  case Nil => List(i)
  case h::t => {
    if (i <= h)
      i::h::t
    else
      h::insert(i, t)
  }
}

//def insert(i: Int, ints: List[Int]): List[Int] = ints match {
//  case Nil => List(i)
//  case h::t => {
//    if (i <= h)
//      i::h::t
//    else
//      isort(h::i::t)
//  }
//}

def qsort[T <% Ordered[T]](list: List[T]): List[T] = list match {
  case Nil => Nil
  case h::Nil => List(h)
  case h::t => {
    val (less, more) = t.partition(e => e < h)
    qsort(less) ::: h :: qsort(more)
  }
}

def msort[T <% Ordered[T]](list: List[T]): List[T] = {
  val halfLen = list.length/2
  list match {
    case Nil => List()
    case h::Nil => list
    case h::t => {
      def merge[T <% Ordered[T]](xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, Nil) => List()
          case (xs, Nil) => xs
          case (Nil, ys) => ys
          //case (List(x), List(y)) => if (x < y) List(x, y) else List(y, x)
          case (xsh::xst, ysh::yst) => {
            if (xsh < ysh) xsh :: merge(ys, xst)
            else  ysh :: merge(yst, xs)
          }
        }
      val (left, right) = list splitAt halfLen
      merge(msort(left), msort(right))
    }
  }
}

// splitAt test
val test0 = List()
val test00 = List(1)
test0 splitAt 10
test00 splitAt 3
val isEmp1 = List() == Nil
val isEmp2 = List() == List[Nothing]
//

val test1 = List(3, 5, 9, 1, 8, -1, 12)

isort(test1)
qsort(test1)
msort(test1)
