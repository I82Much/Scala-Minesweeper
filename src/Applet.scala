import swing._
import java.awt.Color

class MainApplet extends Applet {
  object ui extends UI {
     val mainPanel = new BoxPanel(Orientation.Vertical) {
     // different sort of swing components
     contents.append(new Button("HI"))
     }
     mainPanel.background = Color.WHITE
     contents = mainPanel
   
     def init():Unit = {}
  }
}
