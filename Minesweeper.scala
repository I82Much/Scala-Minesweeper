import java.util.Random
import scala.collection.mutable.ListBuffer
import javax.swing._
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint
import java.awt.Rectangle
import swing.Swing._
import swing.{Frame,Panel,Button,BoxPanel,FlowPanel,Dimension,Orientation}
import java.awt.Color
import java.util.{Observable,Observer}
import swing.event.{WindowClosing}

object Minesweeper {

  def main(args:Array[String]) {
    
    val mineField = new Minefield(10,10,10)
    val scoreboard = new MinefieldScoreboard(mineField)
    val minefieldView = new MinefieldView(mineField)
    
    val view = new BoxPanel(Orientation.Vertical) {
      contents ++ List(scoreboard, minefieldView)
    }

    mineField.addObserver(scoreboard)
    mineField.addObserver(minefieldView)
    
    val frame = new Frame() {
      visible=true
      contents = view
      title = "Minesweeper"
      reactions += {
        case WindowClosing(e) => System.exit(0)
      }
    }
    println(mineField)
  }
}


class MinefieldScoreboard(field:Minefield) extends FlowPanel with Observer {
  import scala.swing._
  import javax.swing.ImageIcon
  
  val timer:Label = new Label("400")
  val reset:Button = new Button() {
    action = new Action("") {
      override def apply():Unit = {
        field.reset()
      }
    }
    icon = new ImageIcon(MinefieldView.happy)
  }
  val numMines:Label = new Label(field.numFlags.toString)
  contents ++ List(timer, reset, numMines)
  // override def contents() = List(timer, reset, numMines)
  
  override def update(o:Observable, arg:Any):Unit = {
    numMines.text = field.numFlags().toString
    
    reset.icon = 
      if (field.isDead) {
        new ImageIcon(MinefieldView.sad)
      }
      else if (field.hasWon) {
        new ImageIcon(MinefieldView.cool)
      }
      else {
        new ImageIcon(MinefieldView.happy)
      }
  }
}
object TextPlacer {
  import java.awt.font.{TextLayout}
  import java.awt.{Font}
  import java.awt.geom.Rectangle2D
  
  object AnchorPoint extends Enumeration {
    type AnchorPoint = Value
    val UpperLeft, TopCenter, UpperRight, RightCenter, BottomRight, BottomCenter, BottomLeft, LeftCenter, Center = Value
  }
  
  def drawText(text:String, point:AnchorPoint.Value, g2:Graphics2D, x:Float, y:Float):Unit = {
    val layout:TextLayout = new TextLayout(text, g2.getFont(), g2.getFontRenderContext())
    drawText(layout, point, g2, x, y)
  }
  
  def drawText(text:TextLayout, point:AnchorPoint.Value, g2:Graphics2D, x:Float, y:Float):Unit = {
    val bounds:Rectangle2D = text.getBounds()
    val midYOffset = (bounds.getHeight()/2).asInstanceOf[Float]
    val midXOffset = (-bounds.getWidth()/2).asInstanceOf[Float]

    val topYOffset = bounds.getHeight().asInstanceOf[Float]
    val bottomYOffset = 0.0f

    val leftXOffset = 0.0f
    val rightXOffset = -bounds.getWidth().asInstanceOf[Float]

    // Adjust x values
    val translationX = point match {
        // Left
        case AnchorPoint.UpperLeft => leftXOffset
        case AnchorPoint.BottomLeft => leftXOffset
        case AnchorPoint.LeftCenter => leftXOffset
        // Mid
        case AnchorPoint.TopCenter => midXOffset
        case AnchorPoint.BottomCenter => midXOffset
        case AnchorPoint.Center => midXOffset;
        // Right
        case AnchorPoint.UpperRight => rightXOffset
        case AnchorPoint.RightCenter => rightXOffset
        case AnchorPoint.BottomRight => rightXOffset
    }

    // Adjust y values
    val translationY = point match {
        // Top
        case AnchorPoint.UpperLeft    => topYOffset
        case AnchorPoint.UpperRight   => topYOffset
        case AnchorPoint.TopCenter    => topYOffset
        // Mid
        case AnchorPoint.LeftCenter   => midYOffset
        case AnchorPoint.Center       => midYOffset
        case AnchorPoint.RightCenter  => midYOffset
        // Bottom
        case AnchorPoint.BottomLeft   => bottomYOffset
        case AnchorPoint.BottomCenter => bottomYOffset
        case AnchorPoint.BottomRight  => bottomYOffset
    }
    text.draw(g2, x + translationX, y + translationY);
  }
    
  
  
  
}


object MinefieldView {
  import java.awt.image.BufferedImage
  import javax.imageio.ImageIO
  import java.io.File
  val iconHome = "icons/"
  val flag:BufferedImage = ImageIO.read(new File(iconHome + "red_flag_32.png"))
  val bomb:BufferedImage = ImageIO.read(new File(iconHome + "bomb_128.png"))
  val happy:BufferedImage = ImageIO.read(new File(iconHome + "happy.png"))
  val cool:BufferedImage = ImageIO.read(new File(iconHome + "cool.png"))
  val sad:BufferedImage = ImageIO.read(new File(iconHome + "sad.png"))
}

class MinefieldView(field:Minefield) extends Panel with Observer {
  import Minestatus._
  import ExplorationStatus._
  import scala.swing.event.{MouseDragged,MousePressed,MouseReleased}
  import java.awt.Point
  import TextPlacer._
  import java.awt.geom.AffineTransform
  import java.awt.image.BufferedImage
  import java.awt.event.MouseEvent
  import java.awt.event.InputEvent

  
  val squareSize = MinefieldView.flag.getWidth()
  val numRows = field.numRows()
  val numCols = field.numCols()
  val mines = field.mineStatus()
  preferredSize = new Dimension(squareSize * field.numCols(), squareSize * field.numRows())
  
  val unexploredColor = Color.BLUE
  val exploredColor = Color.GRAY.brighter()
  val gridLineColor = Color.WHITE
  
  
  var middleDown = false
  var middlePoint:Tuple2[Int,Int] = (0,0)
  
  var leftDown = false
  var leftPoint:Tuple2[Int,Int] = (0,0)
  
  var curPoint:Tuple2[Int,Int] = (0,0)
  
  listenTo(mouse.clicks, mouse.moves)
  
  override def update(o:Observable, arg:Any):Unit = {
    repaint()
  }
  
  def pointToColumnRow(point:Point):Tuple2[Int,Int] = {
    val width = size.width
    val height = size.height
    val gridWidth = 1.0 * width / numCols
    val gridHeight = 1.0 * height / numRows

    val col = (point.x / gridWidth).asInstanceOf[Int]
    val row = (point.y / gridHeight).asInstanceOf[Int]
    println ("row " + row + " col " + col)
    (col,row)
  }
  
  
  // TODO: Listen for right clicks to cycle through
  reactions += {
    case MouseDragged(src, point, mods) => {
      if (middleDown) { 
        middlePoint = (point.x, point.y)
      }
      curPoint = (point.x, point.y)
      repaint() 
    }
    case MouseReleased(src, point, i1, i2, b) => mouseReleased(point, i1, i2, b)
    case MousePressed(src, point, i1, i2, b) => handleMousePress(point, i1, i2, b)
    // case e => println("=> "+e.toString)
  }
  
  def mouseReleased(point:Point, modifiers:Int, clicks:Int, triggersPopup:Boolean):Unit = {
    val colRow = pointToColumnRow(point)
    
    if (leftDown && sameSquare(leftPoint, (point.x, point.y))) {
      field.expand(colRow._1, colRow._2)
    }
    
    // If the middle is down, attempt to expand all of them if it's unambiguous
    if (middleDown) {
      for (coord <- field.adjacentCoordinates(colRow._1, colRow._2)) {
       field.expand(coord._1, coord._2)
      }
    }
    
    
    middleDown = false
    leftDown = false

    repaint()
  }
  
  def handleMousePress(point:Point, modifiers:Int, clicks:Int, triggersPopup:Boolean):Unit = {
    val colRow = pointToColumnRow(point)
    
    // Determine if both left and right buttons pressed: http://www.daniweb.com/forums/thread123929.html
    val onMask = InputEvent.BUTTON1_DOWN_MASK & InputEvent.BUTTON3_DOWN_MASK
    val offMask = InputEvent.BUTTON1_DOWN_MASK | InputEvent.BUTTON3_DOWN_MASK
    if ( (modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0) {
      println("Left down")
      leftDown = true
      leftPoint = (point.x,point.y)
      curPoint = leftPoint
      repaint()
    }
    if ((modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0) {
      println("2 down")
    }
    if ((modifiers & InputEvent.BUTTON3_DOWN_MASK) != 0) {
      println("3 down")
    }
    
    
    if ( (modifiers & (onMask | offMask)) == onMask) {
      middleDown = true
      middlePoint = (point.x, point.y)
      println(middlePoint)
      repaint()
    }
    
    
    if (triggersPopup) {
      field.toggleFlag(colRow._1, colRow._2)
    }
    
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

  /**
   * Determine if the mouse coordinates refer to the same grid square 
   */
  private def sameSquare(mx1:Int, my1:Int, mx2:Int, my2:Int):Boolean = {
    pointToColumnRow(mx1, my1) == pointToColumnRow(mx2, my2)
  }

  private def sameSquare(p1:Tuple2[Int,Int], p2:Tuple2[Int,Int]):Boolean = {
    sameSquare(p1._1, p1._2, p2._1, p2._2)
  }
  
  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    val width = size.getWidth().asInstanceOf[Int]
    val height = size.getHeight().asInstanceOf[Int]
    
    val gridWidth = width / numCols
    val gridHeight = height / numRows
    
    val numberColorMap = Map(1->Color.BLUE, 2->Color.GREEN.darker(), 3->Color.RED, 4->Color.BLACK)
    
    // the y locs of row i are at i and i + 1.  Similarly for column
    val rowGridLines:List[Int] = (0 until numRows + 1).toList.map(map(_, 0, numRows, 0, height-1).asInstanceOf[Int])
    val colGridLines:List[Int] = (0 until numCols + 1).toList.map(map(_, 0, numCols, 0, width-1).asInstanceOf[Int])
    g.clearRect(0,0,width,height)
    
    def rect(x:Int, y:Int):Rectangle = {
      val (x1, y1) = (colGridLines(x), rowGridLines(y))
      val (x2, y2) = (colGridLines(x+1), rowGridLines(y+1))
      val (width, height) = (x2-x1, y2-y1)
      new Rectangle(x1,y1,width,height)
    }
    
    // Don't color these differently
    // val ignored = List(ExplorationStatus.Flagged, ExplorationStatus.Question)
    
    val toColorDifferently:List[Tuple2[Int,Int]] = 
      if (middleDown) {
        val center = pointToColumnRow(middlePoint._1, middlePoint._2)
        (center :: field.adjacentCoordinates(center._1, center._2))//.filter(!ignored.contains(_))
      }
      // Show the currently pressed square as a diff color, as long as the mouse
      // resides within it
      else if (leftDown && sameSquare(leftPoint, curPoint) && !ignored.contains(curPoint)) {
        List(pointToColumnRow(leftPoint._1, leftPoint._2))
      }
      else {
        Nil
      }
    

    def drawUnexplored(x:Int, y:Int):Unit = {
      if (toColorDifferently.contains(x,y)) {
        g.setColor(Color.ORANGE)
      }
      else {
        g.setColor(unexploredColor)
      }
      val bounds = rect(x,y)
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
    }
    
    def drawIcon(bounds:Rectangle, icon:BufferedImage):Unit = {
      val xScale = bounds.width / icon.getWidth().asInstanceOf[Float]
      val yScale = bounds.height / icon.getHeight().asInstanceOf[Float] 
      // Scale, then translate
      val scale = AffineTransform.getScaleInstance(xScale, yScale)
      val translate = AffineTransform.getTranslateInstance(bounds.x, bounds.y)
      
      translate.concatenate(scale)
      
      g.drawImage(icon, translate, null)
      
    }
    
    def drawMine(x:Int, y:Int):Unit = {
      g.setColor(Color.RED)
      val bounds = rect(x,y)
      drawIcon(bounds, MinefieldView.bomb)
    }
    
    
    
    def drawExplored(x:Int, y:Int):Unit = {
      g.setColor(exploredColor)
      val bounds = rect(x,y)
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
      
      if (field.isMine(x,y)) {
        drawMine(x,y)
      }
      else {
        val numAdjacent = field.numAdjacent(x,y)
        if (numAdjacent != 0) {
          g.setColor(numberColorMap.getOrElse(numAdjacent, Color.RED))
          val cx = bounds.x + bounds.width/2
          val cy = bounds.y + bounds.height/2
          TextPlacer.drawText(numAdjacent.toString, AnchorPoint.Center, g, cx, cy)
        }
      }
    }
    def drawFlag(x:Int, y:Int):Unit = {
      drawUnexplored(x,y)
      val bounds = rect(x,y)
      val cx = bounds.x + bounds.width/2
      val cy = bounds.y + bounds.height/2
      val icon = MinefieldView.flag
      
      drawIcon(bounds, icon)
    }
    def drawQuestion(x:Int, y:Int):Unit = {
      drawUnexplored(x,y)
      val bounds = rect(x,y)
      g.setColor(Color.WHITE)
      val cx = bounds.x + bounds.width/2
      val cy = bounds.y + bounds.height/2
      TextPlacer.drawText("?", AnchorPoint.Center, g, cx, cy)
    }

    for (x <- 0 until numCols) {
      for (y <- 0 until numRows) {

        // val status:ExplorationStatus = field.explorationStatus(x,y)
        field.explorationStatus(x,y) match {
          case ExplorationStatus.Unexplored => drawUnexplored(x,y)
          case ExplorationStatus.Explored => drawExplored(x,y)
          case ExplorationStatus.Flagged => drawFlag(x,y)
          case ExplorationStatus.Question => drawQuestion(x,y)
        }
      }
    }
    
    // Draw the grid lines
    g.setColor(gridLineColor)

    // Draw horizontal lines
    for (row <- 0 until numRows + 1) {
      val y = rowGridLines(row)
      g.drawLine(0,y,width,y)
    }
    // Vertical lines
    for (col <- 0 until numCols + 1) {
      val x = colGridLines(col)
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
  val Unexplored, Explored, Flagged, Question = Value
}


class Minefield(width:Int, height:Int, numMines:Int) extends Observable {
  import Minestatus._
  import ExplorationStatus._
  private val random = new Random()
  
  type Coord = Tuple2[Int,Int]
  
  // Can either move forward, stay put, or move back
  private val moves = List(1,0,-1)
  private val adjacent8Directions = (for (x<-moves; y<-moves) yield(x,y)).filter(x=>x!=(0,0))
  
  def adjacentCoordinates(x:Int, y:Int) = adjacent8Directions.map(loc=>(x+loc._1, y+loc._2)).filter(!outOfBounds(_))
  
    
  private var numFlagsRemaining:Int = numMines
  
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

  private def calculateMineLocations():List[Coord] = {
    pickRandomCoordinates(numMines, 0, 0, width, height)
  }
  
  private var mineLocs = calculateMineLocations()
  
  private def fromLocations(coords:List[Coord]):Array[Array[Minestatus.Value]] = {
    // 2d array of Minestatus
    val mines = Array.fill(height,width)(Minestatus.Safe)
    // we have x,y, change to row, column
    coords.foreach(x => mines(x._2)(x._1) = Minestatus.Dangerous)
    mines
  }
  
  private def initField() = { Array.fill(height,width)(ExplorationStatus.Unexplored)}
  
  private var mines:Array[Array[Minestatus.Value]] = fromLocations(mineLocs)
  
  
  
  private var field:Array[Array[ExplorationStatus.Value]] = initField()
  
  def explorationStatus(x:Int, y:Int) = field(y)(x)
  
  def mineStatus() = mines
 
  def numRows() = height
  def numCols() = width
  
  
  private var dead = false
  def isDead() = dead
  
  def changed() = {
    setChanged()
    notifyObservers()
  }

  def reset() {
    dead = false
    numFlagsRemaining = numMines
    mineLocs = calculateMineLocations()
    mines = fromLocations(mineLocs)
    field = initField()
    adjacentCounts = calculateNumAdjacentMines()
    changed()
  }

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
  
  var adjacentCounts = calculateNumAdjacentMines()

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
    if (outOfBounds(x,y) || 
        previouslyVisited.contains(x,y) ||  
        explorationStatus(x,y) == ExplorationStatus.Flagged || 
        explorationStatus(x,y) == ExplorationStatus.Question) { 
          return Unit 
    }
      
      
    val coord = (x,y)
    previouslyVisited.append(coord)
    toExpand.append(coord)
    
    if (numAdjacent(x,y) != 0) {
      return Unit
    }
  
    for (dir <- adjacent8Directions) {
      val x1 = x + dir._1
      val y1 = y + dir._2
      if (!previouslyVisited.contains((x1,y1))) {
        expandEmptySpace(toExpand, previouslyVisited, x1, y1)
        
      }
    }
  }
  
  
  def isMine(x:Int, y:Int):Boolean = {
    mines(y)(x) == Minestatus.Dangerous
  }
  
  def numFlags() = {
    // If it's negative, return 0
    numFlagsRemaining.max(0)
  }
  
  def toggleFlag(x:Int, y:Int):Unit = {
    val curExploration = explorationStatus(x,y)
    // Three states for flagging - unexplored, flagged, question.  Cycles through
    // these
    field(y)(x) = curExploration match {
      case ExplorationStatus.Unexplored => { numFlagsRemaining -= 1; ExplorationStatus.Flagged }
      case ExplorationStatus.Flagged => { numFlagsRemaining += 1; ExplorationStatus.Question }
      case ExplorationStatus.Question => ExplorationStatus.Unexplored
      case _ => field(y)(x)
    }
    changed()
  }
  
  def kill(): Unit = {
    dead = true
    // expand everything
    for (row <- 0 until numRows) {
      for (col <- 0 until numCols) {
        field(row)(col) = ExplorationStatus.Explored
      }
    }
  }
  
  // TODO: Refactor this
  def expand(x:Int, y:Int):Unit = {
    // If they click on a ? or Flag, ignore it
    val ignored = List(ExplorationStatus.Flagged, ExplorationStatus.Question)
    if (ignored.contains(explorationStatus(x,y))) {
      return Unit
    }
    
    
    // If they clicked on a mine, it's game over
    if (isMine(x,y)) {
      kill()
      changed()
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
    }
      
    if (numAdjacent(x,y) > 0) {
      expandNonZeroSquare()
    }
    else {
      expandZeroSquare()
    }
    
    changed()
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
  
  private var won = false
  // TODO: calculate when they've won
  
  def hasWon():Boolean = {
    // See if all of the 
    mineLocs.forall(loc => explorationStatus(loc._1, loc._2) == ExplorationStatus.Flagged)
  }
  
}

