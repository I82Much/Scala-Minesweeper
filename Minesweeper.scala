import java.util.Random
import scala.collection.mutable.ListBuffer
import javax.swing._
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint
import swing.Swing._
import swing.{Frame,Panel,Dimension}
import java.awt.Color

object Minesweeper {

  
  def main(args:Array[String]) {
    
    val mineField = new Minefield(5,5,10)
    val view = new MinefieldView(mineField)
    
    val frame = new Frame() {
      visible=true
      contents = view
      title = "Minesweeper"
      // reactions += {
      //         x => System.exit(0)
      //         // case WindowClosing(e) => System.exit(0)
      //       }
    }
    println(mineField)
  
  }
}


class MinefieldView(field:Minefield) extends Panel {
  import Minestatus._
  
  val squareSize = 20
  val numRows = field.numRows()
  val numCols = field.numCols()
  val mines = field.mineStatus()
  preferredSize = new Dimension(20 * field.numCols(), 20 * field.numRows())

  /**
   * @param value The incoming value to be converted
   * @param low1  Lower bound of the value's current range
   * @param high1 Upper bound of the value's current range
   * @param low2  Lower bound of the value's target range
   * @param high2 Upper bound of the value's target range
   */
  def map(value:Double, low1:Double, high1:Double, low2:Double, high2:Double):Double = {
    val diff = value - low1
    val proportion = diff / (high1 - low1)
    lerp(low2, high2, proportion)
  }


  // Linearly interpolate between two values
  def lerp(value1:Double, value2:Double, amt:Double):Double = {
      ((value2 - value1) * amt) + value1
  }

  
  override def paint(g: Graphics2D): Unit = {
    super.paint(g)
    
    val width = size.getWidth().asInstanceOf[Int]
    val height = size.getHeight().asInstanceOf[Int]
    
    val gridWidth = width / numCols
    val gridHeight = height / numRows
    
    g.clearRect(0,0,width,height)
    
    g.setColor(Color.RED)
    // Draw the mines
    for (x <- 0 until numCols) {
      for (y <- 0 until numRows) {
        val x1 = gridWidth * x
        val y1 = gridHeight * y
        if (mines(y)(x) == Minestatus.Dangerous) {
          // Draw an x
          g.drawLine(x1,y1,x1+gridWidth,y1+gridHeight)
          g.drawLine(x1,y1+gridHeight,x1+gridWidth,y1)
        }
        // Draw the number 
        else {
          g.drawString(String.valueOf(field.numAdjacent(x,y)), x1+gridWidth/2, y1+gridHeight/2)
        }
      }
    }
    
    val debug = true
    if (debug) {
      g.setColor(new Color(0,0,255,128))
    }
    else {
      g.setColor(Color.BLUE);
    }
  
    g.fillRect(0,0,width,height)
    
    // Draw the grid lines
    g.setColor(Color.WHITE)
    // g.drawRect(0,0,width,height)
    // Draw horizontal lines
    for (row <- 0 until numRows + 1) {
      val y = map(row, 0, numRows, 0, height-1).asInstanceOf[Int]
      g.drawLine(0,y,width,y)
    }
    // Vertical lines
    for (col <- 0 until numCols + 1) {
      val x = map(col, 0, numCols, 0, width-1).asInstanceOf[Int]
      g.drawLine(x,0,x,height)
    }
  }
  
}


object Minestatus extends Enumeration {
  val Safe = Value("Safe")
  val Dangerous = Value("Dangerous")
}

class Minefield(width:Int, height:Int, numMines:Int) {
  import Minestatus._
  val random = new Random()
  
  // Calculates random (x,y) pairs in the given range
  private def pickRandomCoordinates(numCoords:Int, minX:Int, minY:Int, maxX:Int, maxY:Int):List[Tuple2[Int,Int]] = {
    var coords:ListBuffer[Tuple2[Int,Int]] = new ListBuffer()
    for (i <- 0 until numCoords) {
      def randomBetween(lowerBound:Int, upperBound:Int):Int = {
        val diff = upperBound - lowerBound
        lowerBound + random.nextInt(diff)
      }
      var found = false
      while (!found) {
        val x = randomBetween(minX, maxX)
        val y = randomBetween(minY, maxY)
        val tuple = (x,y)
        if (!coords.contains(tuple)) {
          found = true
          coords += tuple
        }
      }
    }
    coords.toList
  }

  private def calculateMineLocations():Array[Array[Minestatus.Value]] = {
    val mineCoords = pickRandomCoordinates(numMines, 0, 0, width, height)
    // 2d array of Minestatus
    val mines = Array.fill(height,width)(Minestatus.Safe)
    // we have x,y, change to row, column
    mineCoords.foreach(x => mines(x._2)(x._1) = Minestatus.Dangerous)
    mines
  }
  
  private val mines:Array[Array[Minestatus.Value]] = calculateMineLocations()
  
  def mineStatus() = mines
  // private def subRectArray(array:Array[Array[E]], x:Int, y:Int, width:Int, height:Int) = {
  //   val numRows = array.length
  //   val numCols = array(0).length
  //   // Array.fill(width, height)
  //   
  //   null
  //   
  //   
  // }

  def numRows() = height
  def numCols() = width

  private def calculateNumAdjacentMines():Array[Array[Int]] = {
    
    // Simple algorithm.  Loop through all adjacent squares for each square
    // on the board, and check how many of those are mines.
    // To avoid special case logic at edges and corners, we extend the board
    // one square in each direction
    val scratchBoard = Array.fill(width+2,height+2)(Minestatus.Safe)
    
    // Copy the interior section of the board
    var counter = 0
    for (row <- mines) {
      // Skip the first empty row
      val scratchRow = scratchBoard(counter+1)
      Array.copy(row, 0, scratchRow, 1, row.length)
      counter += 1
    }
    
    val dirs = List(-1,0,1)
    // crossproduct of the different directions you can take; same as manually
    // calculating the east, southeast, south, southwest, etc
    val adjacent = (for (x<-dirs; y<-dirs) yield(x,y))
    
    // Now loop through and count up
    val answers = Array.fill(width,height)(0)
    for (row <- 0 until height) {
      for (col <- 0 until width) {
        var counter = 0
        for ((x,y) <- adjacent) {
          // Need to add 1 to row and col to make up for the extra space
          val scratchRow = row + y + 1
          val scratchCol = col + x + 1
          if (scratchBoard(scratchRow)(scratchCol) == Minestatus.Dangerous) {
            counter += 1
          }
        }
        answers(row)(col) = counter
      }
    }
    println(answers.map(_.mkString(" ")).mkString("\n"))
    answers
  }
  
  val adjacentCounts = calculateNumAdjacentMines()

  def numAdjacent(x:Int, y:Int) = {
    adjacentCounts(y)(x)
  }
  
  override def toString():String = {
    // for all the rows
    val sb = new StringBuilder()
    for (row <- mines) {
      sb.append(row.map(x => if (x==Minestatus.Safe) { "_" } else { "X" } ).mkString(" "))
      sb.append("\n")
    }
    sb.toString()
  }
  
  
}

