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
    val t3 = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of another large tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of another large tree") {
    new TestTrees {
      assert(chars(t1) === List('a','b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times for a List of chars") {
    assert(times(List('a','a','c','a','b','b')) === List(('a',3),('c',1),('b',2)))
  }

  test("times for a null List") {
    assert(times(Nil) === List())
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assert(makeOrderedLeafList(List(('a', 3), ('b', 10), ('c', 2))) === List(Leaf('c',2), Leaf('a',3), Leaf('b',10)))
  }
  
  test("singleton checks if List[CodeTree] contains only a single code tree") {
    assert(singleton(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))) === true)
    assert(singleton(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("until singleton returns tree") {
    val single_tree = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
    val trees = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Fork(Leaf('a',2),Leaf('b',3),List('a','b'),5))
    assert(until(singleton,combine)(single_tree) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
    assert(until(singleton,combine)(trees) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Fork(Leaf('a',2),Leaf('b',3),List('a','b'),5), List('e','t','a','b'),8)))
  }

  test("newly created tree") {
    assert(createCodeTree(string2Chars("bbbaaaaett")) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Fork(Leaf('b',3),Leaf('a',4),List('b','a'),7), List('e','t','b','a'),10))
  }
  
  test("decoded secret has a length of 14 chars") {
    assert(decodedSecret.length === 14)
  } 

  test("decoded secret is 'huffmanestcool'") {
    assert(decodedSecret === List('h','u','f','f','m','a','n','e','s','t','c','o','o','l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("blah") {
    val t = createCodeTree(string2Chars("bbbaaaaett"))
    assert(decode(t, encode(t)("btbatba".toList)) === "btbatba".toList)
  }

  test("decode and encode 'huffmanestcool'") {
    new TestTrees {
      assert(decode(t3, encode(t3)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
  
  test("decode and quick-encode 'huffmanestcool'") {
    new TestTrees {
      assert(decode(t3, quickEncode(t3)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
}
