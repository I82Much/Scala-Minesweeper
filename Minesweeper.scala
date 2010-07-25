import java.util.Random
import scala.collection.mutable.ListBuffer
import javax.swing._
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint
import java.awt.Rectangle
import swing.Swing._
import swing.{Frame,Panel,Dimension}
import java.awt.Color

object Minesweeper {

  
  def main(args:Array[String]) {
    
    println("Num mines: ")
    // val numMines = Console.readInt()
    
    val mineField = new Minefield(5,5,5)
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
  
    // while (true) {
    //       val args = Console.readLine().split(",")
    //       val x = args(0).trim().toInt
    //       val y = args(1).trim().toInt
    //       
    //       
    //       println("Expanded values for (" + x + ", " + y + ")")
    //       val coords = mineField.expandEmptySpace(x,y)
    //       println("Size " + coords.size)
    //       println(coords.mkString("\n"))
    //     }
  
  }
}





class MinefieldView(field:Minefield) extends Panel {
  import Minestatus._
  import ExplorationStatus._
  import scala.swing.event.{MouseDragged,MousePressed}
  import java.awt.Point
  
  val squareSize = 20
  val numRows = field.numRows()
  val numCols = field.numCols()
  val mines = field.mineStatus()
  preferredSize = new Dimension(20 * field.numCols(), 20 * field.numRows())

  listenTo(mouse.clicks, mouse.moves)
  
  def pointToColumnRow(point:Point):Tuple2[Int,Int] = {
    val width = size.width
    val height = size.height
    val gridWidth = 1.0 * width / numCols
    val gridHeight = 1.0 * height / numRows

    val col = (point.x / gridWidth).asInstanceOf[Int]
    val row = (point.y / gridHeight).asInstanceOf[Int]
    println ("Clicking on row " + row + " col " + col)
    (col,row)
  }
  
  
  // TODO: Listen for right clicks to cycle through
  reactions += {
    // case MouseDragged(src, point, mods) => println("mouse dragged")
    case MousePressed(src, point, i1, i2, b) => handleMousePress(point, i1, i2, b)
    // case e => println("=> "+e.toString)
  }
  
  def handleMousePress(point:Point, modifiers:Int, clicks:Int, triggersPopup:Boolean):Unit = {
    val colRow = pointToColumnRow(point)
    if (triggersPopup) {
      field.toggleFlag(colRow._1, colRow._2)
    }
    else {
      field.expand(colRow._1, colRow._2);
    }
    repaint()
  }

  

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
    
    val numColMap = Map(1->Color.BLUE, 2->Color.GREEN, 3->Color.RED, 4->Color.RED)
    
    
    g.clearRect(0,0,width,height)
    
    val unexploredColor = Color.BLUE
    val exploredColor = Color.GRAY.brighter()
    
    def rect(x:Int, y:Int):Rectangle = {
      new Rectangle(gridWidth * x, gridHeight * y, gridWidth, gridHeight)
    }

    def drawUnexplored(x:Int, y:Int):Unit = {
      g.setColor(unexploredColor)
      val bounds = rect(x,y)
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
    }
    
    def drawExplored(x:Int, y:Int):Unit = {
      g.setColor(exploredColor)
      val bounds = rect(x,y)
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
      val numAdjacent = field.numAdjacent(x,y)
      if (numAdjacent != 0) {
        g.setColor(numColMap.getOrElse(numAdjacent, Color.RED))
        g.drawString(numAdjacent.toString, bounds.x + bounds.width/2, bounds.y + bounds.height/2)
      }
    }
    def drawFlag(x:Int, y:Int):Unit = {
      drawUnexplored(x,y)
      val bounds = rect(x,y)
      g.setColor(Color.RED)
      g.drawString("F", bounds.x + bounds.width/2, bounds.y + bounds.height/2)
    }
    def drawQuestion(x:Int, y:Int):Unit = {
      drawUnexplored(x,y)
      val bounds = rect(x,y)
      g.setColor(Color.BLACK)
      g.drawString("?", bounds.x + bounds.width/2, bounds.y + bounds.height/2)
    }
    // g.setColor(Color.RED)
    // Draw the mines
    for (x <- 0 until numCols) {
      for (y <- 0 until numRows) {
        val x1 = gridWidth * x
        val y1 = gridHeight * y
        
        val status:ExplorationStatus = field.explorationStatus(x,y)
        status match {
          case ExplorationStatus.Unexplored => drawUnexplored(x,y)
          case ExplorationStatus.Explored => drawExplored(x,y)
          case ExplorationStatus.Flagged => drawFlag(x,y)
          case ExplorationStatus.Question => drawQuestion(x,y)
        }
        
        
        // if (mines(y)(x) == Minestatus.Dangerous) {
        //   // Draw an x
        //   g.drawLine(x1,y1,x1+gridWidth,y1+gridHeight)
        //   g.drawLine(x1,y1+gridHeight,x1+gridWidth,y1)
        // }
        // // Draw the number 
        // else {
        //   g.drawString(String.valueOf(field.numAdjacent(x,y)), x1+gridWidth/2, y1+gridHeight/2)
        // }
      }
    }
    
    // val debug = true
    //     if (debug) {
    //       g.setColor(new Color(0,0,255,128))
    //     }
    //     else {
    //       g.setColor(Color.BLUE);
    //     }
    //   
    //     g.fillRect(0,0,width,height)
    
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

object ExplorationStatus extends Enumeration {
  type ExplorationStatus = Value
  val Unexplored = Value("Unexplored")
  val Explored = Value("Explored")
  val Flagged = Value("Flagged")
  val Question = Value("Question")
  // Cycle between UnexploredWithNumber, Flagged, Question when right clicking
  // on appropriate square
}


class Minefield(width:Int, height:Int, numMines:Int) {
  import Minestatus._
  import ExplorationStatus._
  private val random = new Random()
  
  type Coord = Tuple2[Int,Int]
  
  // Can either move forward, stay put, or move back
  private val moves = List(1,0,-1)
  private val adjacent8Directions = (for (x<-moves; y<-moves) yield(x,y)).filter(x=>x._1 != 0 || x._2 != 0)

  private val adjacent4Directions = List((1,0), (0,1), (-1,0), (0,-1))
    
  
  // Calculates random (x,y) pairs in the given range
  private def pickRandomCoordinates(numCoords:Int, minX:Int, minY:Int, maxX:Int, maxY:Int):List[Coord] = {
    var coords:ListBuffer[Coord] = new ListBuffer()
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
  
  private val field:Array[Array[ExplorationStatus.Value]] = Array.fill(height,width)(ExplorationStatus.Unexplored)
  
  def explorationStatus(x:Int, y:Int) = field(y)(x)
  
  def mineStatus() = mines
 
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
    
    // Now loop through and count up
    val answers = Array.fill(width,height)(0)
    for (row <- 0 until height) {
      for (col <- 0 until width) {
        var counter = 0
        for ((x,y) <- adjacent8Directions) {
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

  private def expandEmptySpace(x:Int, y:Int):List[Coord] = {
    // If it's not on a zero to start with, there are no additional
    // spaces that need to be uncovered
    if (numAdjacent(x,y) != 0) {
      return List[Coord]((x,y))
    }
    else {
      val buff = new ListBuffer[Coord]()
      val previouslyVisited = new ListBuffer[Coord]()
      buff.append((x,y))
      previouslyVisited.append((x,y))
      
      
      for (dir <- adjacent8Directions) {
        val x1 = x + dir._1
        val y1 = y + dir._2
        expandEmptySpace(buff, previouslyVisited, x1, y1)
      }
      
      // // Expand north
      // expandEmptySpace(buff, previouslyVisited, x,y-1)
      // // Expand east
      // expandEmptySpace(buff, previouslyVisited, x+1,y)
      // // expand south
      // expandEmptySpace(buff, previouslyVisited, x,y+1)
      // // expand west
      // expandEmptySpace(buff, previouslyVisited, x-1,y)
      return buff.toList
    }
  }
  
  private def outOfBounds(x:Int, y:Int):Boolean = {
    x < 0 || x >= numCols ||
    y < 0 || y >= numRows
  }
  
  private def outOfBounds(c1:Coord):Boolean = {
    outOfBounds(c1._1, c1._2)
  }
  
  private def expandEmptySpace(toExpand:ListBuffer[Coord], previouslyVisited:ListBuffer[Coord], x:Int, y:Int):Unit = {
      
    // Stop the recursion
    if (outOfBounds(x,y) || previouslyVisited.contains(x,y) ){ return Unit }
      
      
    val coord = (x,y)
    previouslyVisited.append(coord)
    toExpand.append(coord)
    
    if (numAdjacent(x,y) != 0) {
      return Unit
    }

    
    // println("Expanding empty space at " + coord)
    //    println("toExpand: " + toExpand)
    //    println("previously visited: " + previouslyVisited)
    
    
    // val north = (0,-1)
    // val east = (1,0)
    // val south = (0,1)
    // val west = (-1,0)
    // val dirs = List(north,east,south,west)
    
    for (dir <- adjacent8Directions) {
      val x1 = x + dir._1
      val y1 = y + dir._2
      if (!previouslyVisited.contains((x1,y1))) {
        expandEmptySpace(toExpand, previouslyVisited, x1, y1)
        
      }
    }
  }
  
  
  private def isMine(x:Int, y:Int):Boolean = {
    mines(y)(x) == Minestatus.Dangerous
  }
  
  def toggleFlag(x:Int, y:Int):Unit = {
    val curExploration = explorationStatus(x,y)
    // Three states for flagging - unexplored, flagged, question.  Cycles through
    // these
    field(y)(x) = curExploration match {
      case ExplorationStatus.Unexplored => ExplorationStatus.Flagged
      case ExplorationStatus.Flagged => ExplorationStatus.Question
      case ExplorationStatus.Question => ExplorationStatus.Unexplored
      case _ => field(y)(x)
    }
  }
  
  // 
  def expand(x:Int, y:Int):Unit = {
    // If they clicked on a mine, it's game over
    if (isMine(x,y)) {
      println("you're dead")
      return Unit
    }
    
    
    def expandZeroSquare(): Unit = {
      field(y)(x) = ExplorationStatus.Explored
      
      // All the squares to reveal
      val expanded = expandEmptySpace(x,y)
      for (toExpand <- expanded) {
        val x1 = toExpand._1
        val y1 = toExpand._2
        field(y1)(x1) = ExplorationStatus.Explored
      }
    }
    def expandNonZeroSquare(): Unit = {
      field(y)(x) = ExplorationStatus.Explored

      // Only expand adjacent squares IF one of the adjacent squares next to it
      // is already explored
      // val adjacent = adjacent8Directions.map(d => (d._1 + x, d._2 + y))
      //       if (adjacent.exists(d => !outOfBounds(d) && explorationStatus(d._1, d._2) == ExplorationStatus.Explored)) {
      //         // Check 8 areas around them to see if they need reveal
      //         revealAdjacentHiddenNumbers(x,y)
      //       }
      
    }
    
    
    
    // def revealAdjacentHiddenNumbers(x:Int, y:Int) {
    //       for (offset <- adjacent8Directions) {
    //         val x2 = x + offset._1
    //         val y2 = y + offset._2
    //         
    //         if (!outOfBounds(x2, y2) && explorationStatus(x2,y2) == ExplorationStatus.UnexploredHidden) {
    //           field(y2)(x2) = ExplorationStatus.UnexploredWithNumber
    //         }
    //       }
    //     }
    
    if (numAdjacent(x,y) > 0) {
      expandNonZeroSquare()
    }
    else {
      expandZeroSquare()
    }
    
  }
  
  
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

