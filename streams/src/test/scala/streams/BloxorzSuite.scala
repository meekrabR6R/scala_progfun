package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }

  test("block is standing") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(1,2))
      assert(b1.isStanding)
    }
  }

  test("block is not standing") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(2,2))
      assert(!b1.isStanding)
    }
  }

  test("block position is legal") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(2,2))
      assert(b1.isLegal)
    }
  }

  test("block position is not legal") {
    new Level1 {
      val b1 = Block(Pos(2,0), Pos(3,0))
      assert(!b1.isLegal)
    }
  }

  test("Start block") {
    new Level1 {
      assert(startBlock === Block(Pos(1,1), Pos(1,1)))
    }
  }

  test("Not Start block") {
    new Level1 {
      assert(startBlock != Block(Pos(2,1), Pos(2,1)))
    }
  }

  test("block neighbors") {
    new Level1 {
      val b1 = Block(Pos(1,2), Pos(2,2))
      val neighbors = List((Block(Pos(1,1),Pos(2,1)), Left), (Block(Pos(1,3),Pos(2,3)), Right),
                           (Block(Pos(0,2),Pos(0,2)), Up), (Block(Pos(3,2),Pos(3,2)), Down))
      assert(b1.neighbors == neighbors)
    }
  }

  test("legal block neighbors") {
    new Level1 {
      val b1 = Block(Pos(2,0), Pos(2,0))
      assert(b1.legalNeighbors == List((Block(Pos(2,1),Pos(2,2)), Right), (Block(Pos(0,0),Pos(1,0)), Up)))
    }
  }

  test("is done 1") {
    new Level1 {
      assert(done(Block(Pos(4,7),Pos(4,7))))
    }
  }

  test("is done 2") {
    new Level1 {
      assert(done(Block(Pos(3,7),Pos(4,7))))
    }
  }

  test("is not done") {
    new Level1 {
      assert(!done(Block(Pos(2,7),Pos(3,7))))
    }
  }

  test("updated history stream") {
    new Level1 {
      val b1 = Block(Pos(1,1),Pos(1,1))

      val moves = List(Left,Up)
      val neighborSet = Set(
                          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
                          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
                        )
      assert(neighborsWithHistory(b1, moves).toSet === neighborSet) 
    }
  }

  test("only new neighbors") {
    new Level1 {
      val newNeighbors = newNeighborsOnly(
                          Set(
                            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
                            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
                          ).toStream,
                          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
                        )
      val newNeighborsMatch = Set(
                                (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
                              ).toStream

      assert(newNeighbors === newNeighborsMatch)
    }
  }

  ignore("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  ignore("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
