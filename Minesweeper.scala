import java.util.Random

object Minesweeper {

  
  def main(args:Array[String]) {
    
    val mineField = new Minefield(5,5,10)
    println(mineField)
  
  }
}

object Minestatus extends Enumeration {
  val Safe = Value("Safe")
  val Dangerous = Value("Dangerous")
}

class Minefield(width:Int, height:Int, numMines:Int) {
  import Minestatus._
  
  
  val random = new Random()
  
  
  
  
  // Start as all empty
  private val mines = Array.fill(height,width)(Minestatus.Safe)
  
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

