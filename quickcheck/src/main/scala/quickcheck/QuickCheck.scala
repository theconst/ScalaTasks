package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  // generates the arbitrary {non-empty} heaps
  lazy val genHeap: Gen[H] = {
      arbitrary[Int].flatMap(k => oneOf(const(empty), genHeap).map(heap => insert(k, heap)))
  }
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  // generates a pair of different integers
  lazy val difPair = {
    for(first <- arbitrary[Int];
        second <- arbitrary[Int] suchThat (_ != first)) yield (first, second)
  }

  //copied from the example
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //copied from the example
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("twoelem1") = forAll(difPair) {
     (p: (Int, Int)) => { 
        var h = insert(p._1, insert(p._2, empty))
        val i = findMin(h)
        h = deleteMin(h)
        val j = findMin(h)
        h = deleteMin(h)
        (j == Math.max(i, j)) && (i != j) && (isEmpty(h))
     }
  }
  
   // checks if the heap is sorted
   property("sorted") = forAll(genHeap) {
     def isSorted(heap: H, x: Option[Int]): Boolean = {
      if (isEmpty(heap)) {
        true  
      } else {
        x match {
          case None => true
          case Some(prev) => {
            val next = findMin(heap)
            (prev <= next) && isSorted(deleteMin(heap), Some(next))
          }
        }
      }
    }
    (h: H) => isSorted(h, None)
   }

   
   // checks various properties about melding
   property("melding") = forAll(genHeap, genHeap) {
     (firstHeap: H, secondHeap: H) => (
       Math.min(findMin(firstHeap), findMin(secondHeap)) == findMin(meld(firstHeap, secondHeap)) &&
       Math.min(findMin(firstHeap), findMin(secondHeap)) == findMin(meld(secondHeap, firstHeap)) &&
       findMin(deleteMin(meld(firstHeap, secondHeap))) == findMin(deleteMin(meld(secondHeap, firstHeap)))
     )
   }
   
   
   //counterexample for bogus 4
   // in this heap the whole first tree was deleted
   // sorted test couldn't find the error, since findMin worked correcly
   // and sortedTest didn't fail
   // the bug was only discovered after comparing the code
   property("bruteForce") = forAll(const(true)) {
     (b: Boolean) => {
       val q = insert(4, insert(2, insert(1,empty)))
       
       findMin(deleteMin(q)) == 2
     }
   }
   
}
