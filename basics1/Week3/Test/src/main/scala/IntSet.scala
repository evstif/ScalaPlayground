abstract class IntSet {
  def add(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(set: IntSet): IntSet
}

object Empty extends IntSet {
  def add(x: Int): IntSet = new NonEmpty(x)
  def contains(x: Int): Boolean = false
  def union(set: IntSet): IntSet = set

  override def toString: String = "."
}

class NonEmpty(val root: Int, left: IntSet, right: IntSet) extends IntSet {
  def this(root: Int) = this(root, Empty, Empty)

  def add(x: Int): IntSet = {
    if (x < root) new NonEmpty(root, left add x, right)
    else if (x > root) new NonEmpty(x, left, right add root)
    else this
  }

  def contains(x: Int): Boolean = {
    if (x < root) left contains x
    else if (x > root) right contains x
    else true
  }

  def union(set: IntSet): IntSet = {
    ((left union right) union set) add root
  }

  override def toString: String = "{" + left + root + right + "}"
}