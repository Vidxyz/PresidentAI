package ui.panels

import java.awt.{Color, Dimension}

import ui.models.ComputerPlayer

import scala.swing.{Graphics2D, Panel, SimpleSwingApplication, Swing}

object ComputerPlayerAvatarPanel {
  val width = 300
  val height = 300
}

class ComputerPlayerAvatarPanel(app: SimpleSwingApplication) extends Panel {
  import ComputerPlayerAvatarPanel._

  background = Color.white
  preferredSize = new Dimension(width, height)
  val computerPlayerUi = ComputerPlayer(app)
  border = Swing.LineBorder(Color.BLACK)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    computerPlayerUi.drawSprite(g)
  }

}
