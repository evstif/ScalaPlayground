package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.

  property("findMinOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))

    findMin(h) == Math.min(a, b)
  }

  // If you insert an element into an empty heap,
  // then delete the minimum, the resulting heap should be empty.

  property("emptyAfterDelete") = forAll { (a: Int) =>
    val h = insert(a, empty)

    deleteMin(h) == empty
  }

  property("emptyAfterDeleteOfTwo") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == Math.max(c, Math.max(a, b))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)

  property("seqOfDeleteMinIsSorted") = forAll { (h: H) =>
    def getListByDeleteMin(list: List[Int], h: H): List[Int] = {
      if (isEmpty(h))
        list
      else
        getListByDeleteMin(findMin(h) :: list, deleteMin(h))
    }
    def isListSorted(list: List[Int], result: Boolean = true): Boolean = list match {
      case Nil => true
      case h::t => t match {
        case h1::t1 if h >= h1 => isListSorted(h1::t1, true)
        case h1::_ if h < h1 => false
        case Nil => true
      }
    }

    val list = getListByDeleteMin(List.empty[Int], h)
    //println(s"iter on $list")
    isListSorted(list)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

  property("oneOfMinsAfterMelding") = forAll{ (h1:H, h2:H) =>
    if (isEmpty(h1) && isEmpty(h2))
      true
    else {
      val min1 = if (isEmpty(h1)) 0 else findMin(h1)
      val min2 = if (isEmpty(h2)) 0 else findMin(h2)

      List(min1, min2) contains findMin(meld(h1, h2))
    }
  }
}
