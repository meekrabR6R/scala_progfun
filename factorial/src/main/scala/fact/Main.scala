package fact

object Main {
  def main(args: Array[String]) {
    println(fact(10))
  }

  def fact(n: Int): Int = {
    def loop(acc: Int, n: Int): Int = 
      if (n == 0) acc else loop(acc * n, n - 1)
    loop(1, n)
  }
}
