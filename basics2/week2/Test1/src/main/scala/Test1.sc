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