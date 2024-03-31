object KnightTour {

  // Initial position of Knight
  var x = 0
  var y = 0

  // Define the chessboard size
  val S = 8

  // A 2D chessboard
  var chessboard = Array.ofDim[Int](S, S)

  // Move patterns of Knight
  val dx = Array(2, 1, -1, -2, -2, -1, 1, 2)
  val dy = Array(1, 2, 2, 1, -1, -2, -2, -1)

  // Function to check if the move is valid
  def isValid(x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 && x < S && y < S && chessboard(x)(y) == -1
  }

  // Monad implementation
  case class StateMonad[A](run: (Int, Int) => (A, (Int, Int))) {
    def flatMap[B](f: (A, (Int, Int)) => StateMonad[B]): StateMonad[B] = StateMonad((x, y) => {
      val (a, newState) = run(x, y)
      f(a, newState).run(newState._1, newState._2)
    })
  }

  // Function for Knight's Tour
  def knightTour(movei: Int): StateMonad[Unit] = StateMonad { (x, y) =>
    chessboard(x)(y) = movei
    if (movei == S * S - 1) {
      for (i <- chessboard; j <- i) print(s"$j ")
      println()
      ((), (x, y))
    } else {
      val newStates = for {
        i <- dx.indices
        newX = x + dx(i)
        newY = y + dy(i)
        if isValid(newX, newY)
      } yield knightTour(movei + 1).run(newX, newY)

      val result = newStates.find(_._1 == ())
      result match {
        case Some(((), newState)) => ((), newState)
        case None =>
          chessboard(x)(y) = -1
          ((), (x, y))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    for (i <- chessboard.indices; j <- chessboard(i).indices) chessboard(i)(j) = -1
    val result = knightTour(0).run(x, y)._1
    if (result == ()) println("Solution Found!") else println("Solution not Found!")
  }
}