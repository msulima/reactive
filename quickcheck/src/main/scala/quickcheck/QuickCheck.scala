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

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("remove1") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m = if (findMin(h1) < findMin(h2)) findMin(h1) else findMin(h2)
    findMin(meld(h1, h2)) == m
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
