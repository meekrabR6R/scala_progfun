package fact

import org.scalatest.FunSuite

class FactSuite extends FunSuite {
  import Main.fact

  test("fact: n=10") {
  	assert(fact(10) === 3628800)
  }

  test("fact: n=0") {
  	assert(fact(0) === 1)
  }

  test("fact: n=2") {
    assert(fact(2) === 2)
  }
}