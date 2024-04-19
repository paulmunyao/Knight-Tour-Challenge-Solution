object KnightTour {

  // Define the chessboard size
  val S = 8
  // Move patterns of Knight
  val dx = Array(2, 1, -1, -2, -2, -1, 1, 2)
  val dy = Array(1, 2, 2, 1, -1, -2, -2, -1)
  // Initial position of Knight
  var x = 0
  var y = 0
  // A 2D chessboard
  var chessboard = Array.ofDim[Int](S, S)

  // Function to check if the move is valid
  def isValid(x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 && x < S && y < S && chessboard(x)(y) == -1
  }

  // Function for Knight's Tour
  def knightTour(movei: Int): Unit = {
    if (movei == S * S - 1) {
      for (i <- chessboard; j <- i) print(s"$j ")
      println()
    } else {
      for {
        i <- dx.indices
        newX = x + dx(i)
        newY = y + dy(i)
        if isValid(newX, newY)
      } {
        chessboard(x)(y) = movei
        x = newX
        y = newY
        knightTour(movei + 1)
        chessboard(x)(y) = -1
        x -= dx(i)
        y -= dy(i)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    for (i <- chessboard.indices; j <- chessboard(i).indices) chessboard(i)(j) = -1
    knightTour(0)
    println("Solution Found!")
  }
}







//object KnightTour {
//
//  // Define the chessboard size
//  val S = 8
//  // Move patterns of Knight
//  val dx = Array(2, 1, -1, -2, -2, -1, 1, 2)
//  val dy = Array(1, 2, 2, 1, -1, -2, -2, -1)
//  // Initial position of Knight
//  var x = 0
//  var y = 0
//  // A 2D chessboard
//  var chessboard = Array.ofDim[Int](S, S)
//
//  // Function to check if the move is valid
//  def isValid(x: Int, y: Int): Boolean = {
//    x >= 0 && y >= 0 && x < S && y < S && chessboard(x)(y) == -1
//  }
//
//  // Function for Knight's Tour
//  def knightTour(movei: Int): Unit = {
//    if (movei == S * S - 1) {
//      for (i <- chessboard; j <- i) print(s"$j ")
//      println()
//    } else {
//      for {
//        i <- dx.indices
//        newX = x + dx(i)
//        newY = y + dy(i)
//        if isValid(newX, newY)
//      } {
//        chessboard(x)(y) = movei
//        x = newX
//        y = newY
//        knightTour(movei + 1)
//        chessboard(x)(y) = -1
//        x -= dx(i)
//        y -= dy(i)
//      }
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    for (i <- chessboard.indices; j <- chessboard(i).indices) chessboard(i)(j) = -1
//    knightTour(0)
//    println("Solution Found!")
//  }
//}
