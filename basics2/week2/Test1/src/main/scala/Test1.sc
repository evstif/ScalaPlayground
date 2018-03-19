val mapWithSameKeysInConstructor = Map(1 -> "one", 2 -> "two", 1 -> "one1")

val list = List(1, 2)
val secondElement = list match {
  case x :: Nil => -1
  case x :: xs ⇒ xs.head
  case _ ⇒ 0
}

secondElement

case class Dog(name: String, breed: String)
val d1 = Dog("Scooby", "Doberman")
d1.toString


import java.math.BigInteger
implicit def Int2BigIntegerConvert(value: Int): BigInteger =
  new BigInteger(value.toString)

def add(a: BigInteger, b: BigInteger) = a.add(b)

add(Int2BigIntegerConvert(3), Int2BigIntegerConvert(6)) == Int2BigIntegerConvert(9)
add(3, 6) == 9
add(3, 6) == Int2BigIntegerConvert(9)
add(3, 6) == (9: BigInteger)
add(3, 6).intValue == 9

trait B {
  def bId = 2
}

trait A { self: B =>
  def aId = 1
}

//val a = new A  //***does not compile!!!***
val obj = new A with B

val g: String = "Check out the big brains on Brad!"

g indexOf ('o',7)

(31 toHexString)


val xs = List("Manny", "Moe", "Jack")
val ys = List("Manny", "Moe", "Jack")
(xs sameElements ys)

val xt = List("Manny", "Moe", "Jack")
val yt = List("Manny", "Jack", "Moe")
(xt sameElements yt)

val xs1 = Set(3, 2, 1, 4, 5, 6, 7)
val ys1 = Set(7, 2, 1, 4, 5, 6, 3)
(xs1 sameElements ys1)

val xt1 = Set(1, 2, 3)
val yt1 = Set(3, 2, 1)
(xt1 sameElements yt1)

val set2 = Set(1, 9, 10, 22)
val list2 = List(3, 4, 5, 10)
set2 ++ list2
list2 ++ set2

List(List(1), List(2, 3, 4), List(5, 6, 7), List(8, 9, 10)).flatMap(_.map(_ * 4))

List(1, 2, 3, 4, 5).flatMap(it ⇒ if (it % 2 == 0) Some(it) else None)

List(4, 6, 7, 8, 9, 13, 14).collect {
  case x: Int if (x % 2 == 0) ⇒ x * 3
}

val list3 = List(4, 6, 7, 8, 9, 13, 14)
val partialFunction1: PartialFunction[Int, Int] = {
  case x: Int if x % 2 == 0 ⇒ x * 3
}
val partialFunction2: PartialFunction[Int, Int] = {
  case y: Int if y % 2 != 0 ⇒ y * 4
}
val result = list3.collect(partialFunction1 orElse partialFunction2)

(((((0 - 5) - 4) - 3) - 2) - 1)


(0 /: List(5, 4, 3, 2, 1)) { (`running total`, `next element`) ⇒
  `running total` - `next element`
}

List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Doe").reduceRight {
  _ + _
}
List("Do", "Re", "Me", "Fa", "So", "La", "Te", "Doe").reduceLeft {
  _ + _
}

List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).transpose

List(List(1), List(4)).transpose

def calc(x: () ⇒ Int): Either[Throwable, Int] = {
  try {
    Right(x()) //An explicit call of the x function
  } catch {
    case b: Throwable ⇒ Left(b)
  }
}

val y = calc { () ⇒ //Having explicitly declaring that Unit is a parameter with ()
  14 + 15
}

def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
  "%d %ss can give you %s".format(x, y, z.mkString(", "))
}

repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol"))

classOf[String].getSimpleName

