package ui.panels

import java.awt.Color

import scala.swing.{Dimension, Panel, Swing}

object EmptyFillerPanel {
  val width  = 250
  val height = 300
}

class EmptyFillerPanel extends Panel {
  import EmptyFillerPanel._

  preferredSize = new Dimension(width, height)
  background = Color.white
//  border = Swing.LineBorder(Color.BLACK)

}
