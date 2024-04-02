import scala.annotation.tailrec

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
  def isValid(x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && x < S && y < S && chessboard(x)(y) == -1

  // Dotty-cps-async style
  import scala.scalanative.loop

  def knightTour(movei: Int): Unit = loop {
    chessboard(x)(y) = movei
    if movei == S * S - 1 then
    for i <- chessboard; j <- i do print(s"$j ")
    println()
    else
    val newStates = dx.indices.flatMap { i =>
      val newX = x + dx(i)
      val newY = y + dy(i)
      if isValid(newX, newY) then
        knightTour(movei + 1)
      x = newX
      y = newY
      false
      else
      true
    }
    if newStates.forall(identity) then
      chessboard(x)(y) = -1
  }

  // Direct style
  def knightTourDirect(movei: Int): Boolean = {
    chessboard(x)(y) = movei
    if movei == S * S - 1 then
    for i <- chessboard; j <- i do print(s"$j ")
    println()
    true
    else {
      @tailrec
      def findSolution(indices: Iterator[Int]): Boolean =
        if indices.isEmpty then
          chessboard(x)(y) = -1
      false
      else
      val i = indices.next()
      val newX = x + dx(i)
      val newY = y + dy(i)
      if isValid(newX, newY) then
      val oldX = x
      val oldY = y
      x = newX
      y = newY
      val found = knightTourDirect(movei + 1)
      if found then true else {
        x = oldX
        y = oldY
        findSolution(indices)
      }
      else
      findSolution(indices)

      findSolution(dx.indices.iterator)
    }
  }

  def main(args: Array[String]): Unit = {
    for i <- chessboard.indices; j <- chessboard(i).indices do chessboard(i)(j) = -1
    val result = knightTourDirect(0)
    if result then println("Solution Found!") else println("Solution not Found!")
  }
}