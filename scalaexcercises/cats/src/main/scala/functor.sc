import cats.{Apply, Functor}
import cats.implicits._

Functor[Option].map(fa = None: Option[String])(_.length)

val lenOption: Option[String] ⇒ Option[Int] = Functor[Option].lift(_.length)
lenOption(Some("Hello"))

val source = List("Cats", "is", "awesome")
val product = Functor[List].fproduct(source)(_.length).toMap

List(1, 2, 3).traverse(i => Some(i): Option[Int])

val intToString: Int ⇒ String = _.toString
val double: Int ⇒ Int = _ * 2
val addTwo: Int ⇒ Int = _ + 2

Apply[Option].map(Some(1))(intToString)
Apply[Option].map(Some(1))(double)
Apply[Option].map(None)(addTwo)

val listOpt = Apply[List] compose Apply[Option]
val plusOne = (x: Int) ⇒ x + 1
listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))

Apply[Option].ap(Some(intToString))(Some(1))

val option2 = Option(1) |@| Option(2)