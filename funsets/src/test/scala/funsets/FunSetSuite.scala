package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1      = singletonSet(1)
    val s2      = singletonSet(2)
    val s3      = singletonSet(3)
    val s4      = singletonSet(4)
    val s5      = singletonSet(5)
    val s7      = singletonSet(7)
    val s10     = singletonSet(10)
    val s100    = singletonSet(100)
    val sneg10  = singletonSet(-10)
    val sneg100 = singletonSet(-100)
    val s999    = singletonSet(999)
    val s1000   = singletonSet(1000)

    val s = union(s1, s2)
    val t = union(s2, s3)
    val u = union(s1, s3)
    val v = union(s4, s5)
    val w = union(s7, s1000)
    val q = union(s3, s4)
    val sub1 = union(s1, s10)
    val sub2 = union(s100, sneg10)
    val sub3 = union(sneg100, s999)
    val sub4 = union(s2, s4)
    val superSub1 = union(sub1, sub2)
    val superSub2 = union(sub3, sub4)

    val set = union(superSub1, superSub2)
    val otherSub1 = union(u, v)
    val set2 = union(otherSub1, w)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1), (2), (3) contain 1,2,3 respectively") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton Set 1")
      assert(contains(s2, 2), "Singleton Set 2")
      assert(contains(s3, 3), "Singleton Set 3")
    }
  }

  test("union contains all elements") {
    new TestSets {

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only intersecting elements") {
    new TestSets {
            
      val x = intersect(s, t)
      val y = intersect(t, u)

      assert(contains(x, 2), "Intersect x")
      assert(contains(y, 3), "Intersect y")
    }
  }

  test("diff contains the difference between two sets") {
    new TestSets {

      val x = diff(s, t)
      val y = diff(t, u)
      
      val set3 = union(s, q)

      val diff1 = diff(set2, set3)
      
      assert(contains(x, 1), "Diff x")
      assert(!contains(x, 3), "Diff x")
      assert(!contains(x, 2), "Diff x")
      assert(!contains(y, 1), "Diff y")
      assert(contains(y, 2), "Diff y")
      assert(!contains(y, 3), "Diff y")
      assert(!contains(diff1, 2), "diff1 should not have 2")
      assert(contains(diff1, 5), "diff1 should have 5")
      assert(contains(diff1, 7), "diff1 should have 7")
      assert(contains(diff1, 1000), "diff1 should have 1000")
    }
  }

  test("filter satisfies predicate") {
    new TestSets {
      
      val set1 = union(s, s3)

      val x = filter(set1, x => x == 2)
      val y = filter(set1, x => x < 3)
      
      val z = filter(set2, x => x < 5)
   

      assert(contains(x, 2), "Filter: 2")
      assert(!contains(x, 1), "Filter: 2")
      assert(!contains(x, 3), "Filter: 2")

      assert(contains(y, 1), "Filter: < 3")
      assert(contains(y, 2), "Filter: < 3")
      assert(!contains(y, 3), "Filter: < 3")

      assert(!contains(z, -1), "{1,3,4,5,7,1000")

    }
  }

  test("forall checks if all integers in a set adhere to predicate") {
    new TestSets {
      
      assert(forall(set, x => x < 1000), "x < 1000")
      assert(forall(set, x => x > -1000), "x > -1000")
      assert(!forall(set, x => (x >= 0 && x <= 10)), "x >= 0 && x <= 10")
      assert(!forall(set, x => x > 1000), "x > 1000")
      assert(!forall(set, x => !(x == -10)), "x == -10")
      assert(!forall(set, x => !(x == 999)), "x == 999")
    }
  }

  test("exist ensures that at least one element in Set s adheres to predicate") {
    new TestSets {

      assert(exists(set, x => x == 2), "x == 2")
      assert(exists(set, x => x == 999), "x == 999")
      assert(exists(set, x => x == -10), "x == -10")
      assert(!exists(set, x => x == -20), "x == -20")
    }
  }

  test("map converts given Set into another") {
    new TestSets {
      val newSetOne = map(set, x => x+1)

      assert(contains(newSetOne, 5), "new set contains 5")
      assert(contains(newSetOne, 11), "new set contains 11")
      assert(contains(newSetOne, 101), "new set contains 101")
      assert(contains(newSetOne, 1000), "new set contains 1000")
      assert(!contains(newSetOne, 12), "new set doesn't contain 12")
      assert(!contains(newSetOne, 50), "new set doesn't contain 50")
      assert(!contains(newSetOne, -403), "new set contains -403")
    }
  }
}
