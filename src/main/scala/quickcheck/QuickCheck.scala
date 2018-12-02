package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min 1 item") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("min 2 items") = forAll { (a: Int, b: Int) =>
    val min = math.min(a, b)
    val h2 = insert(b, insert(a, empty))
    findMin(h2) == min
  }

  property("delete min") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("melding min") = forAll { (h1: H, h2: H) =>
    val min = math.min(findMin(h1), findMin(h2))
    findMin(meld(h1, h2)) == min
  }

  property("continuously deleting min") = forAll { h: H =>
    @tailrec
    def findAndDelete(h1: H, values: List[A]): List[A] = {
      if (isEmpty(h1)) values
      else findAndDelete(deleteMin(h1), values :+ findMin(h1))
    }

    val values = findAndDelete(h, List())
    values == values.sorted
  }

  property("min after insert element gt min of a random heap should yield min") = forAll { (a: Int, b: Int, c: Int) =>
    val maximus = List(a, b, c).sorted
    val h1 = insert(a, insert(b, insert(c, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(maximus(1), insert(maximus(2), empty))
  }
}
