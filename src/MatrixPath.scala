/**
  * :File: MatrixPath.scala
  * :Author(s): <eetu.vilkki@aalto.fi>
  * :Date: <20.11.2015>
  **/

import scala.io.Source
import java.io.PrintWriter
import scala.collection.immutable.TreeMap

object MatrixPath extends App {

  // Read the input file
  val lines = Source.fromFile("in.txt").getLines.toArray
  val matrixSize = lines(0).toInt
  val matrix = Array.ofDim[Int](matrixSize, matrixSize)
  val dist = Array.ofDim[Int](matrixSize, matrixSize)
  var sortedMap = new TreeMap[(Int, Int), (Int, Int)]()
  val searchedNodes = Array.fill[Int](matrixSize, matrixSize)(0)
  var l = 1
  for (y <- 0 until matrixSize) {
    val line = lines(y + 1).split(" ")
    for (x <- 0 until matrixSize) {
      matrix(y)(x) = line(x).toInt
      dist(y)(x) = Int.MaxValue
    }
  }
  dist(0)(0) = matrix(0)(0)
  sortedMap += (dist(0)(0), 0) -> (0, 0)

  while (searchedNodes(matrixSize - 1)(matrixSize - 1) < 2) {
    val current = sortedMap.head._2
    sortedMap = sortedMap.tail
    val currentY = current._1
    val currentX = current._2
    searchedNodes(currentY)(currentX) = 2

    for (k <- 0 until 4) {
      var newX = 0
      var newY = 0

      k match {
        case 0 =>
          newY = currentY - 1
          newX = currentX
        case 1 =>
          newY = currentY + 1
          newX = currentX
        case 2 =>
          newY = currentY
          newX = currentX + 1
        case 3 =>
          newY = currentY
          newX = currentX - 1
      }
      if (newX >= 0 && newX < matrixSize && newY >= 0 && newY < matrixSize && searchedNodes(newY)(newX) < 2) {
        if (dist(newY)(newX) > dist(currentY)(currentX) + matrix(newY)(newX)) {
          dist(newY)(newX) = dist(currentY)(currentX) + matrix(newY)(newX)
          if (searchedNodes(newY)(newX) == 1) {
            sortedMap -= sortedMap.find(_._2 == (newY, newX)).get._1
          }
          sortedMap += (dist(newY)(newX), l) -> (newY, newX)
          l += 1
          searchedNodes(newY)(newX) = 1
        }
      }
    }
  }

  val answer = dist(matrixSize - 1)(matrixSize - 1)
  // Form and write the correct output
  val writer = new PrintWriter("out.txt", "UTF-8")
  try writer.print(answer.toString)
  finally writer.close()
}

// EOF
