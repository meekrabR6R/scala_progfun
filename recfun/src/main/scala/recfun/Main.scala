package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], count: Int): Boolean = 
      if (chars.isEmpty) count == 0
      else {
        val head = chars.head
        val numOpen = 
          if (head == '(') count + 1
          else if (head == ')') count - 1
          else count
        if (numOpen >= 0) loop(chars.tail, numOpen)
        else false
      }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money < 0) 0
    else if (money == 0) 1
    else countChange(money-coins.head, coins) +
         countChange(money, coins.tail)
  }
}
