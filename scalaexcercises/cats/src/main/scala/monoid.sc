import cats.{Functor, Monoid, Semigroup}
import cats.implicits._

val l = List(1, 2, 3, 4, 5)
l.foldMap(identity)
l.foldMap(i ⇒ i.toString)

val l2 = List(1, 2, 3, 4, 5)
l2.foldMap(i ⇒ (i, i.toString))

