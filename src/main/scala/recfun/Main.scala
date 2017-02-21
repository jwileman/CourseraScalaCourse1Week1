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
    def pascal(col: Int, row: Int): Int = {
      require(row >= 0, "'row' should be >= 0")
      require(col >= 0, "'col' should be >= 0")
      require(col <= row, "'col' cannot exceed 'row'")

      if (col == 0) 1 //far left number
      else if (col == row) 1 //far right number
      else pascal(col - 1, row - 1) + pascal(col, row - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isBalanced(chars: List[Char], openCount: Int): Boolean = {
        if (chars.isEmpty)
          openCount == 0
        else if (chars.head == '(')
          isBalanced(chars.tail, openCount + 1)
        else if (chars.head == ')')
          openCount > 0 && isBalanced(chars.tail, openCount - 1)
        else
          isBalanced(chars.tail, openCount)
      }
      isBalanced(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) //No change given, therefore only one solution
        1
      else if (money < 0 || coins.isEmpty) //(i)Negative money OR (ii)change must be given but there are no coins
        0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins) //without using the first coin + using at least one of the first coin
    }
  }
