package ui.panels

import java.awt.Color

import game.PlayerCompletionStatus

import scala.swing.{Dimension, Font, Graphics2D, Panel}


object PlayerCompletionUserPromptPanel {
  val width = 600
  val height = 200
  val fontName = "TimesRoman"
  val fontSize = 21
  val backgroundColor = new Color(4,6,84)
}

class PlayerCompletionUserPromptPanel(status: PlayerCompletionStatus, message: String) extends Panel {
  import PlayerCompletionUserPromptPanel._

  preferredSize = new Dimension(width, height)
  minimumSize =  new Dimension(width, height)
  background = backgroundColor

  val strings = message.split('\n')

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(Color.red)
    g.setFont(Font(fontName, Font.Bold, fontSize))
    g.drawString(strings.head, 60, height/4)
    g.setFont(Font(fontName, Font.Bold, fontSize - 7))
    strings.drop(1).zipWithIndex.foreach({
      case  (message, index) => g.drawString(message, 15 ,  (index + 2) * height/4)
    })
  }

}
