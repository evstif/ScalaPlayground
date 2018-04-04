import cats.Semigroup
import cats.implicits._

Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6))
Semigroup[Option[Int]].combine(Option(1), Option(2))

Semigroup[Int ⇒ Int]
  .combine({ (x: Int) ⇒
    x + 1
  }, { (x: Int) ⇒
    x * 10
  })
  .apply(6)

val aMap = Map("foo" -> Map("bar" -> 5))
val anotherMap = Map("foo" -> Map("bar" -> 6))
val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
  opt.map(a |+| _).getOrElse(a)

def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
  lhs.foldLeft(rhs) {
    case (acc, (k, v)) => acc.updated(k, optionCombine(v, acc.get(k)))
  }