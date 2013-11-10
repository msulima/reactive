package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val m = if (a < b) a else b
    findMin(insert(a, insert(b, empty))) == m
  }

  property("min3") = forAll { (a: Int, b: Int) =>
    toList(insert(a, insert(b, empty))) == (if (a < b) List(a, b) else List(b, a))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("delete1") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("delete2") = forAll { (h: H) =>
    toList(h).tail == toList(deleteMin(h))
  }

  property("remove3") = forAll { (a: Int, b: Int) =>
    findMin(deleteMin(insert(a, insert(b, empty)))) == (if (a < b) b else a)
  }

  property("sort1") = forAll { (h: H) =>
    isSorted(h)
  }

  property("sort2") = forAll { (h: H) =>
    isSorted(deleteMin(h))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m = min(findMin(h1), findMin(h2))
    findMin(meld(h1, h2)) == m
  }

  property("meld2") = forAll { (a: Int, b: Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    !isEmpty(deleteMin(h)) && isEmpty(deleteMin(deleteMin(h)))
  }

  property("meld3") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("meld4") = forAll { (h1: H, h2: H) =>
    toList(meld(h1, h2)) == toList(meld(h2, h1))
  }

  def isSorted(h: H): Boolean = {
    isSorted(toList(h))
  }

  def isSorted(h: List[A]): Boolean = {
    if (h.isEmpty || h.size == 1)
      true
    else
      h.head <= h.tail.head && isSorted(h.tail)
  }

  def min(a: Int, b: Int) = if (a < b) a else b

  def toList(h: H): List[A] = {
    if (isEmpty(h))
      Nil
    else
      findMin(h) :: toList(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
