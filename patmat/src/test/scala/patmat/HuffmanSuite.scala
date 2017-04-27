package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  // my test
  test("times testing on list of length 3") {
      times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1))
  }
  
  test("times testing on list of length 4") {
      times(List('a', 'b', 'b', 'a')) === List(('a', 2), ('b', 2))
  }
  
  test("times on a string of length 11") {
    times(List('a', 'b', 'r', 'a', 'c', 'a', 'd', 'a', 'b', 'r', 'a')) === List(('a', 5), ('b', 2), ('r', 2), ('c', 1),('d', 1))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  
  test("test of makeOrderedLeafList, times, string2Chars") {
    val leaflist = makeOrderedLeafList(times(string2Chars("aaaaaaaabbbcdefgh")))
    
    assert(leaflist == List(Leaf('c', 1), Leaf('d', 1), Leaf('e', 1),
        Leaf('f', 1), Leaf('g', 1), Leaf('h', 1),  Leaf('b', 3), Leaf('a', 8)))
  }

  test("combine of some leaf list of length 3 (append to head)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of some leaf list of length 3 (append to tail)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(combine(leaflist) === List(Leaf('x', 2), Fork(Leaf('e',1), Leaf('t',2), List('e', 't'),3)))
  }
  
  test("singleton test on singleton") {
    val leaflist = List(Leaf('e', 1))
    assert(singleton(leaflist) === true)
  }
  
  test("singleton test on empty list") {
    assert(singleton(Nil) === false)
  }
  
  test("singleton test on non-empty list") {
    assert(singleton(List(Leaf('e', 1), Leaf('d', 2))) === false)
  }
  
  
  test("create code tree for simple sentence") {
      assert(createCodeTree(string2Chars("aaabbc")) 
          === Fork(
                Fork(Leaf('c', 1), Leaf('b', 2), List('c', 'b'), 3), 
                Leaf('a', 3), List('c', 'b', 'a'), 6))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
 
  test("decode and encode a longer text should be identity") {
     new TestTrees {
      assert(decode(t2, encode(t2)("aabbbdddd".toList)) === "aabbbdddd".toList)
     }
  }
  
  test("quick encode should produce the same result as encode") {
    new TestTrees {
      assert(encode(t2)("aabbbdddd".toList) === quickEncode(t2)("aabbbdddd".toList))
    }
  }
  
  test("quick encode should correctly encode the secret") {
    val secret = Huffman.decodedSecret
    assert(Huffman.quickEncode(Huffman.frenchCode)(secret) === Huffman.secret)
  }

}
