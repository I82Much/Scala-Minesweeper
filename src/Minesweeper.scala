import java.util.Random
import scala.collection.mutable.ListBuffer
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Paint
import java.awt.Rectangle
import swing.Swing._
import swing._
import java.awt.Color
import java.util.{Observable,Observer}
import swing.event.{WindowClosing,MouseDragged,MousePressed,MouseReleased}
import javax.swing.JApplet
import javax.swing.JMenuBar


// TODO: add time handling
// TODO: allow custom images for the mines
// TODO: Create a class encapsulating the row, column stuff
class MinesweeperApplet extends Applet {
  val game = new MinesweeperGame()
  object ui extends UI {
    contents = game.view
    def init():Unit = {}
  }
  val menuBar = new MinesweeperJMenuBar(game.model, game.minefieldView)
  setJMenuBar(menuBar)
}


object MinesweeperGame {
  // http://en.wikipedia.org/wiki/Minesweeper_%28Windows%29
  // Beginner: 8 × 8 or 9 × 9 field with 10 mines
  // Intermediate: 16 × 16 field with 40 mines
  // Expert: 30 × 16 field with 99 mines
  // Custom: Any values from 8 × 8 or 9 × 9 to 30 × 24 field, with 10 to 667 mines
  // [the maximum number of mines allowed for a field of size A × B is [(A − 1) × (B − 1)].
  case class Difficulty(numRows:Int, numCols:Int, numMines:Int) {}
  object Trivial extends Difficulty(8,8,1) {}
  object Beginner extends Difficulty(8,8,10) {}
  object Intermediate extends Difficulty(16,16,40) {}
  object Expert extends Difficulty(16,30,99) {}
  
  def main(args:Array[String]):Unit = {
    val game = new MinesweeperGame()
    val frame = new Frame() with Observer {
      contents = game.view
      title = "Minesweeper"
      reactions += {
        case WindowClosing(e) => System.exit(0)
      }
      menuBar = new MinesweeperMenu(game.model, game.minefieldView)
      visible=true
      def update(x:Observable, y:Any):Unit = {
        pack()
      }
      game.model.deleteObservers()
      game.model.addObserver(this)
      game.model.addObserver(game.scoreboard)
      game.model.addObserver(game.minefieldView)
    }
  }
}

class MinesweeperGame {
  val difficulty = MinesweeperGame.Beginner
  
  val model = new MinesweeperModel(difficulty.numCols, difficulty.numRows, difficulty.numMines)
  
  val scoreboard = new MinesweeperScoreboard(model)
  val minefieldView = new MinesweeperView(model, scoreboard)
  
  val view = new BoxPanel(Orientation.Vertical) {
    contents ++ List(scoreboard, minefieldView)
  }
      
  // Make the Minesweeper model update the views when it changes
  // Order is important - we want the minefield view to be updated before
  // the frame for resizing purposes; the observable uses a stack.
  // model.addObserver(frame)
  model.addObserver(scoreboard)
  model.addObserver(minefieldView)
}

  

/**
* Provides menu options to the player, including choices for changing the
* difficulty and viewing high scores
*/
class MinesweeperMenu(field:MinesweeperModel, view:MinesweeperView) extends MenuBar {
  import MinesweeperGame.Difficulty._
  import javax.swing.JColorChooser
  def setDifficulty(diff:MinesweeperGame.Difficulty):Unit = {
    field.numRows = diff.numRows
    field.numColumns = diff.numCols
    field.numMines = diff.numMines
    field.reset()
  }
  contents ++
    List(
      new Menu("New Game") {
        contents += new MenuItem( Action("Beginner") { setDifficulty(MinesweeperGame.Beginner) } )
        contents += new MenuItem( Action("Intermediate") { setDifficulty(MinesweeperGame.Intermediate) } )
        contents += new MenuItem( Action("Expert") { setDifficulty(MinesweeperGame.Expert) } )
        // contents += new MenuItem( Action("Custom") { Unit/* Launch dialog for custom*/ } )
      },
      // new Menu("High scores") {
      //        contents += new MenuItem( Action("View High Scores") { Unit } )
      //      },
      new Menu("Customize") {
        contents += new MenuItem( Action("Change Background Color") { 
          val color = JColorChooser.showDialog(null, "Pick background color", view.unexploredColor)
          if (color != null) {
            view.unexploredColor = color
            view.repaint
          }
        })
        
        contents += new CheckMenuItem( "Use flowers instead of mines" ) {
          action = Action("Use flowers instead of mines") {
            view.useFlower = this.selected
            view.repaint
          }
          selected = view.useFlower
        } 
      }
    )
}


/**
* Same as the MinesweeperMenu but a JMenuBar instead of MenuBar due to the
* requirement that Applet uses a JMenuBar and not a MenuBar.
*/
class MinesweeperJMenuBar(field:MinesweeperModel, view:MinesweeperView) extends JMenuBar {
  import MinesweeperGame.Difficulty._
  import javax.swing.{Action,AbstractAction}
  import javax.swing.{JMenu,JCheckBoxMenuItem,JColorChooser}
  
  import java.awt.event.ActionEvent
  
  def setDifficulty(diff:MinesweeperGame.Difficulty):Unit = {
    field.numRows = diff.numRows
    field.numColumns = diff.numCols
    field.numMines = diff.numMines
    field.reset()
  }
  
  def difficultyAction(description:String, difficulty:MinesweeperGame.Difficulty):Action = {
    new AbstractAction(description) {
      override def actionPerformed(act:ActionEvent):Unit = {
        setDifficulty(difficulty)
      }
    }
  }
  
  val newGameMenu = new JMenu("New Game")
  newGameMenu.add(difficultyAction("Beginner", MinesweeperGame.Beginner))
  newGameMenu.add(difficultyAction("Intermediate", MinesweeperGame.Intermediate))
  newGameMenu.add(difficultyAction("Custom", MinesweeperGame.Expert))

  add(newGameMenu)
  
  // Allow user to change background color and switch the mines to flowers
  val customizeMenu = new JMenu("Customize")
  val changeBackgroundColor = new AbstractAction("Change Background Color") {
    override def actionPerformed(evt:ActionEvent):Unit = {
      val color = JColorChooser.showDialog(null, "Pick background color", view.unexploredColor)
      if (color != null) {
        view.unexploredColor = color
        view.repaint
      }
    }
  }
  val toggleMines:JCheckBoxMenuItem = new JCheckBoxMenuItem(new AbstractAction("Use flowers instead of mines") {
    override def actionPerformed(evt:ActionEvent):Unit = {
      view.useFlower = toggleMines.getState
      view.repaint
    }
    
  })
  toggleMines.setState(view.useFlower)
  
  customizeMenu.add(changeBackgroundColor)
  customizeMenu.add(toggleMines)
  
  add(customizeMenu)
}



// TODO: show high scores
class HighScorePanel {
  
}


/**
* The scoreboard consists of three parts:
* * A display showing how many seconds have elapsed
* * A button with a friendly smiley face button reflecting state of game, as well
*   as serving as a reset button
* * A counter showing how many flags have been placed (how many mines are remaining)
*/
class MinesweeperScoreboard(field:MinesweeperModel) extends FlowPanel with Observer {
  import scala.swing._
  import javax.swing.ImageIcon
  import java.awt.image.BufferedImage
  import javax.swing.{Timer}
  import java.awt.event.{ActionListener,ActionEvent}
  import java.text.DecimalFormat
  
  val deadIcon = new ImageIcon(MinesweeperView.sad)
  val winIcon = new ImageIcon(MinesweeperView.cool)
  val defaultIcon = new ImageIcon(MinesweeperView.happy)
  val mousePressedIcon = new ImageIcon(MinesweeperView.excited)
  
  val remainingFlagsNumberFormat = new DecimalFormat("00")
  val timeNumberFormat = new DecimalFormat("000")
  
  val timerLabel:Label = new Label(timeNumberFormat.format(0))
  
  // Resets both the board and the timer
  val reset:Button = new Button() {
    action = Action("") { field.reset() }
    icon = defaultIcon
  }
  
  val numFlags:Label = new Label(remainingFlagsNumberFormat.format(field.numFlags))
  
  // This scoreboard consists of the label showing time elapsed, the reset/smiley face
  // icon, and the number of flags that have been placed
  contents ++ List(timerLabel, reset, numFlags)

  // While mouse is pressed, the smiley face becomes an excited face
  def mousePressed():Unit = { reset.icon = mousePressedIcon; Unit }
  def mouseReleased():Unit = {reset.icon = defaultIcon; Unit }
  
  // Model has changed, ensure that all views match.
  override def update(o:Observable, arg:Any):Unit = {
    numFlags.text = remainingFlagsNumberFormat.format(field.numFlags)
    timerLabel.text = timeNumberFormat.format(field.elapsedSeconds)
    reset.icon = 
      if (field.isDead) {
        deadIcon
      }
      else if (field.hasWon) {
        winIcon
      }
      else {
        defaultIcon
      }
  }
}

/**
* Handles drawing strings such that certain parts of them are positioned
* at a given x,y (in screen coordinates).  This makes it very easy to center
* a piece of text at the center of the screen, for instance, and hides the
* complications of calculating string bounds
*/
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
        case AnchorPoint.Center => midXOffset
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

/**
* The Minesweeper view companion object holds references to the art assets 
* needed to draw the game board and scoreboard.
*/
object MinesweeperView {
  import java.awt.image.BufferedImage
  import javax.imageio.ImageIO
  import java.io.File
  val iconHome = "icons/"
  val flag:BufferedImage = ImageIO.read(new File(iconHome + "red_flag_32.png"))
  
  val bomb:BufferedImage = ImageIO.read(new File(iconHome + "bomb_128.png"))
  val flower:BufferedImage = ImageIO.read(new File(iconHome + "Sunflower.png"))
  
  val happy:BufferedImage = ImageIO.read(new File(iconHome + "happy.png"))
  val excited:BufferedImage = ImageIO.read(new File(iconHome + "ooh.png"))
  val cool:BufferedImage = ImageIO.read(new File(iconHome + "cool.png"))
  val sad:BufferedImage = ImageIO.read(new File(iconHome + "sad.png"))
}

/**
* The MinesweeperView is actually a hybrid view/controller of the underlying
* model.  It provides the grid board, as well as handles mouse events to 
* determine what actions need to be taken to the model.  In MVC fashion,
* the controller aspects modify the model, which notifies the view, which
* then updates itself, rather than the view immediately updating as a result
* of a mouse click, for instance.  This allows us to be sure that the view
* that the player sees is consistent with that of the board.
*/
class MinesweeperView(field:MinesweeperModel, scoreboard:MinesweeperScoreboard) extends Panel with Observer {
  import Minestatus._
  import ExplorationStatus._
  import scala.swing.event.{MouseDragged,MousePressed,MouseReleased}
  import java.awt.Point
  import TextPlacer._
  import java.awt.geom.AffineTransform
  import java.awt.image.BufferedImage
  import java.awt.event.MouseEvent
  import java.awt.event.InputEvent

  
  val squareSize = 20
  var numRows = field.numRows
  var numCols = field.numColumns
  
  // Should we display flowers instead of mines upon death
  var useFlower:Boolean = false

  def repack():Unit = {
    preferredSize = new Dimension(squareSize * field.numColumns, squareSize * field.numRows)
    minimumSize = preferredSize
    maximumSize = preferredSize
  }
  
  var unexploredColor = Color.BLUE
  val exploredColor = Color.GRAY.brighter()
  val gridLineColor = Color.BLACK
  val highlightColor = Color.GRAY
  
  val purple = new Color(160,32,240)
  val maroon = new Color(176,48,96)
  val turquoise = new Color(0,206,209)
  
  val numberColorMap = Map(1->Color.BLUE, 2->Color.GREEN.darker(), 3->Color.RED,
    4->purple, 5->maroon, 6->turquoise,7->Color.BLACK,8->Color.GRAY.darker())
  
  var middleDown = false
  var middlePoint:Tuple2[Int,Int] = (0,0)
  
  var leftDown = false
  var leftPoint:Tuple2[Int,Int] = (0,0)
  
  var curPoint:Tuple2[Int,Int] = (0,0)
  
  listenTo(mouse.clicks, mouse.moves)
  
  override def update(o:Observable, arg:Any):Unit = {
    numRows = field.numRows
    numCols = field.numColumns
    repack()
    
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
//    case MouseExited(src, point, modifiers) => 
    // case e => println("=> "+e.toString)
  }
  
  /**
  * When mouse is released, depending on which buttons were pressed, either
  * attempt to expand the 8 adjacent squares around the location of the mouse
  * release, or just the single center space.
  */
  def mouseReleased(point:Point, modifiers:Int, clicks:Int, triggersPopup:Boolean):Unit = {
    
    
    if (field.gameOver()) { return Unit }
    
    // Make sure the scoreboard reacts appropriately by changing the icon back
    // to default
    scoreboard.mouseReleased
    
    val colRow = pointToColumnRow(point)
    
    if (field.inBounds(colRow)) {
    
      if (middleDown) {
        val (x,y) = colRow
        // We don't always expand adjacent, but only when player has unambiguously
        // flagged the squares that he thinks are mines in the vicinity
        if (field.shouldExpandAdjacent(x,y)) {
          field.expandAdjacent(x,y)
        }
      }
      
      else if (leftDown) {
        field.expand(colRow._1, colRow._2)
      }
    }
    
    middleDown = false
    leftDown = false
    
    repaint()
  }
  
  
  /**
  *
  */
  def handleMousePress(point:Point, modifiers:Int, clicks:Int, triggersPopup:Boolean):Unit = {
    if (field.gameOver()) { return Unit }
    
    // Just a fun little touch, when mouse is pressed, change the smiley 
    // face icon in the scoreboard
    scoreboard.mousePressed
    
    val colRow = pointToColumnRow(point)
    if (!field.inBounds(colRow)) { return Unit }
    
    // Determine if both left and right buttons pressed: http://www.daniweb.com/forums/thread123929.html
    val bothMask = InputEvent.BUTTON1_DOWN_MASK | InputEvent.BUTTON3_DOWN_MASK
    
    // Both mice buttons are down, expand the adjacent 8 squares around
    // center, when mouse released
    if ( (modifiers & bothMask) == bothMask) {
      middleDown = true
      middlePoint = (point.x, point.y)
      repaint()
    }
    
    else if ( (modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0) {
      println("Left down")
      leftDown = true
      leftPoint = (point.x,point.y)
      curPoint = leftPoint
      repaint()
    }
    
    else if (triggersPopup) {
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
    
    // the y locs of row i are at i and i + 1.  Similarly for column
    val rowGridLines:List[Int] = (0 until numRows + 1).toList.map(map(_, 0, numRows, 0, height-1).asInstanceOf[Int])
    val colGridLines:List[Int] = (0 until numCols + 1).toList.map(map(_, 0, numCols, 0, width-1).asInstanceOf[Int])
    g.clearRect(0,0,width,height)
    
    /**
    * @return the rectangle (in screen coordinates) for the square at row
    * y and column x in 
    */
    def rect(x:Int, y:Int):Rectangle = {
      val (x1, y1) = (colGridLines(x), rowGridLines(y))
      val (x2, y2) = (colGridLines(x+1), rowGridLines(y+1))
      val (width, height) = (x2-x1, y2-y1)
      new Rectangle(x1,y1,width,height)
    }
    
    // Don't color these differently
    val ignored = List(ExplorationStatus.Flagged, ExplorationStatus.Question)
    
    val pushedIn:List[Tuple2[Int,Int]] = 
      if (middleDown) {
        val center = pointToColumnRow(middlePoint._1, middlePoint._2)
        val adjacentCoords = (center :: field.adjacentCoordinates(center._1, center._2))
        adjacentCoords.filter(loc => field.inBounds(loc) && field.explorationStatus(loc._1,loc._2) == ExplorationStatus.Unexplored)
      }
      // Show the currently pressed square as pushedIn
      else if (leftDown /*&& sameSquare(leftPoint, curPoint)*/ && !ignored.contains(curPoint)) {
        List(pointToColumnRow(curPoint._1, curPoint._2))
      }
      else {
        Nil
      }
      

    val center = pointToColumnRow(curPoint._1, curPoint._2)
    

    def drawUnexplored(x:Int, y:Int):Unit = {
      val pressedIn = pushedIn.contains(x,y)
      if (pressedIn) {
        g.setColor(unexploredColor.darker)
      }
      else {
        g.setColor(unexploredColor)
      }
      val bounds = rect(x,y)
      g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)

      // If pressed in, draw additional black shadow in left and top side
      // of square
      val pressedWidth = if (pressedIn) 1 else 0
      
      g.setColor(Color.black)
      // Vertical left side
      g.fillRect(bounds.x, bounds.y, pressedWidth, bounds.height)
      // Horizontal top
      g.fillRect(bounds.x, bounds.y, bounds.width, pressedWidth)
      
      
      // To make the grid appear 3D, make the square have a highlight at top and left edge
      g.setColor(highlightColor)
      // Upper left
      val (x1, y1) = (bounds.x+pressedWidth+1, bounds.y+pressedWidth+1)
      // lower right
      val (x2, y2) = (bounds.x+bounds.width, bounds.y+bounds.height)
      
      g.drawLine(x1, y1, x2, y1)
      g.drawLine(x1, y1, x1, y2)
      
    }
    
    // Draw a buffered image within the given bounds.  Used
    // to draw the flag and mine icons
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
      if (useFlower) {
        drawIcon(bounds, MinesweeperView.flower)
      }
      else {
        drawIcon(bounds, MinesweeperView.bomb)
      }
    }
    def drawExplored(x:Int, y:Int):Unit = {
      
      val fillColor = 
        if (field.isDetonated(x,y)) {
          Color.RED
        }
        else {
          exploredColor
        }
      
      g.setColor(fillColor)
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
      val icon = MinesweeperView.flag
      
      drawIcon(bounds, icon)
    }
    def drawMisflagged(x:Int, y:Int):Unit = {
      drawFlag(x,y)
      val bounds = rect(x,y)
      // Draw an x through it
      g.setColor(Color.RED)
      val (x1, y1) = (bounds.x, bounds.y)
      val (x2, y2) = (bounds.x + bounds.width, bounds.y + bounds.height)
      
      g.drawLine(x1,y1,x2,y2)
      g.drawLine(x1,y2,x2,y1)
    }
    def drawQuestion(x:Int, y:Int):Unit = {
      drawUnexplored(x,y)
      val bounds = rect(x,y)
      g.setColor(Color.WHITE)
      val cx = bounds.x + bounds.width/2
      val cy = bounds.y + bounds.height/2
      TextPlacer.drawText("?", AnchorPoint.Center, g, cx, cy)
    }

    // Draw the square with the correct type
    for (x <- 0 until numCols) {
      for (y <- 0 until numRows) {
        field.explorationStatus(x,y) match {
          case ExplorationStatus.Unexplored => drawUnexplored(x,y)
          case ExplorationStatus.Explored => drawExplored(x,y)
          case ExplorationStatus.Flagged => drawFlag(x,y)
          case ExplorationStatus.Question => drawQuestion(x,y)
          case ExplorationStatus.Misflagged => drawMisflagged(x,y)
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
  
  repack()
}

object Minestatus extends Enumeration {
  val Safe = Value("Safe")
  val Dangerous = Value("Dangerous")
  val Misflagged = Value("Misflagged")
  val Detonated = Value("Detonated")
}

object ExplorationStatus extends Enumeration {
  type ExplorationStatus = Value
  val Unexplored, Explored, Flagged, Misflagged, Question = Value
}

case class Vector2(rowOffset:Int, colOffset:Int) {
  def +(v:Vector2):Vector2 = {
    Vector2(rowOffset+v.rowOffset, colOffset + v.colOffset)
  }
}
object Vector2 {
  val north = Vector2(-1,0)
  val northEast = Vector2(-1,1)
  val east = Vector2(0,1)
  val southEast = Vector2(1,1)
  val south = Vector2(1,0)
  val southWest = Vector2(1,-1)
  val west = Vector2(-1,0)
  val northWest = Vector2(-1,-1)
  val zero = Vector2(0,0)
  
  val adjacent8 = List(north, northEast, east, southEast, south, southWest, west, northWest)
  val adjacent4 = List(north, east, south, west)
}

case class Coord2(row:Int, col:Int) {
  def +(v:Vector2):Coord2 = {
    Coord2(row + v.rowOffset, col + v.colOffset)
  }
}

abstract case class GameState() {}
class Won extends GameState {}
class InProgress extends GameState {}
class Lost extends GameState {}


/**
* The heart of the whole Minesweeper game, this class handles
* the board state, calculating the random locations of mines, determining
* whether the player has won, lost, or is in progress
*/
class MinesweeperModel(width:Int, height:Int, numFlags:Int) extends Observable {
  import Minestatus._
  import ExplorationStatus._
  import javax.swing.{Timer}
  import java.awt.event.{ActionListener,ActionEvent}
  
  private val random = new Random()
  
  var numRows = height
  var numColumns = width
  var numMines = numFlags
  
  type Coord = Tuple2[Int,Int]

  var elapsedSeconds = 0
  
  val timerTask = new ActionListener() {
    override def actionPerformed(evt:ActionEvent):Unit = {
      elapsedSeconds += 1
      changed()
    }
  }
  val timer = new Timer(1000, timerTask)
  
    
  // Can either move forward, stay put, or move back
  private val moves = List(1,0,-1)
  private val adjacent8Directions = (for (x<-moves; y<-moves) yield(x,y)).filter(x=>x!=(0,0))
  
  def adjacentCoordinates(x:Int, y:Int) = adjacent8Directions.map(loc=>(x+loc._1, y+loc._2)).filter(inBounds(_))
    
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
    pickRandomCoordinates(numMines, 0, 0, numColumns, numRows)
  }
  
  private var mineLocs = calculateMineLocations()
  
  private def fromLocations(coords:List[Coord]):Array[Array[Minestatus.Value]] = {
    // 2d array of Minestatus
    val mines = Array.fill(numRows,numColumns)(Minestatus.Safe)
    // we have x,y, change to row, column
    coords.foreach(x => mines(x._2)(x._1) = Minestatus.Dangerous)
    mines
  }
  
  private def initField() = { Array.fill(numRows,numColumns)(ExplorationStatus.Unexplored)}
  
  private var mines:Array[Array[Minestatus.Value]] = fromLocations(mineLocs)
  
  private var field:Array[Array[ExplorationStatus.Value]] = initField()
  
  // User cannot die on first click.  
  var firstClick = true
  
  def explorationStatus(x:Int, y:Int) = field(y)(x)
  
  def isExplored(x:Int, y:Int) = explorationStatus(x,y) == ExplorationStatus.Explored
  def isFlagged(x:Int, y:Int) = explorationStatus(x,y) == ExplorationStatus.Flagged
  def isQuestion(x:Int, y:Int) = explorationStatus(x,y) == ExplorationStatus.Question
  def isUnexplored(x:Int, y:Int) = explorationStatus(x,y) == ExplorationStatus.Unexplored
  
  def mineStatus() = mines
  
  private var dead = false
  def isDead() = dead
  
  def changed() = {
    setChanged()
    notifyObservers()
  }

  def reset() {
    timer.stop
    elapsedSeconds = 0
    firstClick = true
    dead = false
    won = false
    numFlagsRemaining = numMines
    mineLocs = calculateMineLocations()
    mines = fromLocations(mineLocs)
    field = initField()
    adjacentCounts = calculateNumAdjacentMines()
    changed()
  }

  /**
  * On first click, user cannot die.  So if where they click would cause them
  * to die, we randomly move the mine and recalculate the numbers
  */
  private def moveMineRandomly(x:Int, y:Int):Unit = {
    var foundLoc = false
    while (!foundLoc) {
      val row = random.nextInt(numRows)
      val col = random.nextInt(numColumns)
      
      // Must be an empty square
      if (!isMine(col, row)) {
        foundLoc = true
        mines(y)(x) = Minestatus.Safe
        mines(row)(col)  = Minestatus.Dangerous
        // Recalculate all the adjacencies
        adjacentCounts = calculateNumAdjacentMines()
      }
    }
    Unit
  }

  private def calculateNumAdjacentMines():Array[Array[Int]] = {
    
    // Simple algorithm.  Loop through all adjacent squares for each square
    // on the board, and check how many of those are mines.
    // To avoid special case logic at edges and corners, we extend the board
    // one square in each direction
    val scratchBoard = Array.fill(numRows+2,numColumns+2)(Minestatus.Safe)
    
    // Copy the interior section of the board
    var counter = 0
    for (row <- mines) {
      // Skip the first empty row
      val scratchRow = scratchBoard(counter+1)
      Array.copy(row, 0, scratchRow, 1, row.length)
      counter += 1
    }
    
    // Now loop through and count up
    val answers = Array.fill(numRows,numColumns)(0)
    for (row <- 0 until numRows) {
      for (col <- 0 until numColumns) {
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
  
  def inBounds(x:Int, y:Int): Boolean = {
    (x >= 0 && x < numColumns) &&
    (y >= 0 && y < numRows)
  }
  
  def inBounds(c1:Coord): Boolean = {
    inBounds(c1._1, c1._2)
  }
  
  
  private def expandEmptySpace(toExpand:ListBuffer[Coord], previouslyVisited:ListBuffer[Coord], x:Int, y:Int):Unit = {
      
    // Stop the recursion
    if (!inBounds(x,y) || 
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
    mines(y)(x) != Minestatus.Safe
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
      case ExplorationStatus.Question => {ExplorationStatus.Unexplored}
      case _ => field(y)(x)
    }
    if (playerWon()) {
      win()
    }
    changed()
  }
  
  /**
  * Determines whether the given coordinate has been misflagged; user had
  * a flag in an area that was not a mine
  */
  private def misflagged(c:Coord):Boolean = {
    !isMine(c._1, c._2) && isFlagged(c._1, c._2)
  }
  
  private def lose(x:Int,y:Int): Unit = {
    dead = true
    
    // Reveal the unexplored section
    replace(ExplorationStatus.Unexplored, ExplorationStatus.Explored)

    // If any mines have been misflagged, reveal them as such
    
    replace(misflagged _, ExplorationStatus.Misflagged)
    

    
    mines(y)(x) = Minestatus.Detonated
    timer.stop
  }
  
  private def win(): Unit = {
    won = true
    timer.stop
    numFlagsRemaining = 0
    // TODO: check for high score, prompt user for name if they have fastest time
    
  }
  
  
  private def replace(replaceCondition: Coord => Boolean, replacement:ExplorationStatus.Value):Unit = {
    // TODO: functional way of accomplishing this?
    for (row <- 0 until numRows) {
      for (col <- 0 until numColumns) {
        if (replaceCondition((col,row))) {
          field(row)(col) = replacement
        }
      }
    }
  }
  
  private def replace(target:ExplorationStatus.Value, replacement:ExplorationStatus.Value):Unit = {
    def sameExplorationStatus(cond:Coord):Boolean = {
      val (col, row) = cond
      field(row)(col) == target
    }
    replace(sameExplorationStatus _, replacement)
  }
  
  
  private def fillBoard(status:ExplorationStatus.Value):Unit = {
    field = Array.fill(numRows, numColumns)(status)
  }
  
  /**
   * Determines whether the rules of game permit the alternative mode of
   * clearing mines to occur at this x,y loc.  The rule is simple:
   * have you flagged an equal number of mines in the squares adjacent to
   * the x, y loc as there are potential mines.
   */
  def shouldExpandAdjacent(x:Int, y:Int):Boolean = {
    isExplored(x,y) && {
      val toExpand = adjacentCoordinates(x,y)
      val numFlagsFlaggedAdjacent = toExpand.count(loc=>isFlagged(loc._1,loc._2))
      numFlagsFlaggedAdjacent == numAdjacent(x,y)
    }
  }

  def expandAdjacent(x:Int, y:Int):Unit = {
    val toExpand = adjacentCoordinates(x,y)
    toExpand.foreach(loc => expand(loc._1, loc._2))
  }
  

  // TODO: Refactor this
  def expand(x:Int, y:Int):Unit = {
    if (firstClick) {
      timer.start
    }
    // If they click on a ? or Flag, ignore it
    val ignored = List(ExplorationStatus.Flagged, ExplorationStatus.Question)
    if (ignored.contains(explorationStatus(x,y))) {
      return Unit
    }
    
    // If they clicked on a mine, it's game over, unless it's the first click
    if (isMine(x,y)) {
      if (firstClick) {
        println("Moving mine.")
        moveMineRandomly(x,y)
        println(this.toString)
        expand(x,y)
      }
      else {
        lose(x,y)
        changed()
      }
    }
    
    firstClick = false
    
    // Expands a square with zero adjacent mines; can cause a cascading effect
    // by recursively exploring all the adjacent zero mine squares
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
    
    if (playerWon()) {
      win()
    }
    
    changed()
  }
  

  def isDetonated(x:Int, y:Int) = {
    mines(y)(x) == Minestatus.Detonated
  }
  
  /** @returns the number of adjacent mines */
  def numAdjacent(x:Int, y:Int) = {
    adjacentCounts(y)(x)
  }

  /**
  * @return a 2d representation of the board.
  */
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
  
  private def playerWon():Boolean = {
    // Only non explored or flagged left
    val explorationStatuses = field.flatten
    val numExplored = explorationStatuses.count(_ == ExplorationStatus.Explored)
    val numFlagged = explorationStatuses.count(_ == ExplorationStatus.Flagged)
    val numQuestion = explorationStatuses.count(_ == ExplorationStatus.Question)
    val numUnexplored = explorationStatuses.count(_ == ExplorationStatus.Unexplored)
    
    numQuestion == 0 && numUnexplored + numFlagged == numMines
  }
  
  def hasWon():Boolean = won
  def gameOver():Boolean = won || dead
}

