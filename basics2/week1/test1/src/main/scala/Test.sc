val f: PartialFunction[String, String] = { case "ping" => "pong" }

f("ping")
f.isDefinedAt("abc")

List(42, "cat") collect { case i: Int => i + 1 }

// for :_* see https://stackoverflow.com/a/7938636
def test(string: String): Vector[Vector[Char]] =
  Vector(string.split("\n").map(str => Vector(str: _*)): _*)

test(
  """test1
    |test2
  """.stripMargin)