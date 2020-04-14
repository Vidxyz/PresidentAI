package ui.panels

import java.awt.Color

import scala.swing.{Dimension, Panel, Swing}

object EmptyFillerPanel {
  val width  = 200
  val height = 200
  val backgroundColor = new Color(0,102, 0)
}

class EmptyFillerPanel extends Panel {
  import EmptyFillerPanel._

  preferredSize = new Dimension(width, height)
  background = backgroundColor
//  border = Swing.LineBorder(Color.BLACK)

}
