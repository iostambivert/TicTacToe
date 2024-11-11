import scala.io.StdIn.readLine

class TicTacToe {
  // Initialize the board with empty spaces
  private val board: Array[Array[Char]] = Array.fill(3, 3)(' ')

  // Function to print the board
  def printBoard(): Unit = {
    for (row <- board) {
      println(row.mkString("|"))
      println("-" * 5)
    }
  }

  // Function to check if the current player wins
  def checkWinner(symbol: Char): Boolean = {
    // Check rows, columns, and diagonals
    for (i <- 0 until 3) {
      if (board(i).forall(_ == symbol) || board.map(_(i)).forall(_ == symbol)) {
        return true
      }
    }

    // Check diagonals
    if ((board(0)(0) == symbol && board(1)(1) == symbol && board(2)(2) == symbol) ||
      (board(0)(2) == symbol && board(1)(1) == symbol && board(2)(0) == symbol)) {
      return true
    }

    false
  }

  // Function to check if the board is full
  def isBoardFull(): Boolean = {
    !board.exists(row => row.contains(' '))
  }

  // Function to place a move on the board
  def makeMove(row: Int, col: Int, symbol: Char): Boolean = {
    if (row >= 0 && row < 3 && col >= 0 && col < 3 && board(row)(col) == ' ') {
      board(row)(col) = symbol
      true
    } else {
      println("Invalid move. Try again.")
      false
    }
  }
}

object TicTacToeGame {
  def main(args: Array[String]): Unit = {
    val game = new TicTacToe
    var currentPlayer = 'X'
    var winner = false

    // Game loop
    while (!game.isBoardFull() && !winner) {
      game.printBoard()

      // Ask for the player's move
      println(s"Player $currentPlayer, enter your move (row and column): ")
      val move = readLine().split(" ")
      
      if (move.length == 2) {
        try {
          val row = move(0).toInt
          val col = move(1).toInt
          
          if (game.makeMove(row, col, currentPlayer)) {
            winner = game.checkWinner(currentPlayer)
            if (!winner) {
              currentPlayer = if (currentPlayer == 'X') 'O' else 'X'
            }
          }
        } catch {
          case _: NumberFormatException => println("Please enter valid row and column numbers.")
        }
      } else {
        println("Please enter row and column as two numbers separated by space.")
      }
    }

    game.printBoard()

    if (winner) {
      println(s"Player $currentPlayer wins!")
    } else {
      println("It's a draw!")
    }
  }
}
