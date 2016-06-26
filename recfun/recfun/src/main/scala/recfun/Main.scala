package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceDepth(chars, 0)
  }

  // Helper function, carries number of open parens
  def balanceDepth(chars: List[Char], nOpen: Int): Boolean = {
    if (chars.isEmpty)
      0 == nOpen
    else {
      val first = chars.head
      val rest = chars.tail
      if (first == '(')
        balanceDepth(rest, nOpen + 1)
      else if (first == ')') {
        if (nOpen > 0)
          balanceDepth(rest, nOpen - 1)
        else
          false
      } else
        balanceDepth(rest, nOpen)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    countChangeSorted(money, coins.sorted)
  }
  
  def countChangeSorted(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty)
      0
    else {
      val coin = coins.head
      val rest = coins.tail
      
      // Use highest value coins first
      val nCombos = countChangeSorted(money, rest)
      
      if (coin == money) {
        nCombos + 1
      } else if (coin < money)
        nCombos + countChangeSorted(money - coin, coins)
      else // coin > money
        nCombos
    }
  }
}
