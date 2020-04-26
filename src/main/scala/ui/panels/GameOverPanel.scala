package ui.panels

import java.awt.Color

import scala.swing.{Dimension, Font, Graphics2D, Panel}

class GameOverPanel(playerCompletionOrder: List[String]) extends Panel {
  import GameOverPanel._

  preferredSize = new Dimension(width, height)
  background = backgroundColor


  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    g.setColor(Color.red)
    g.setFont(Font(fontName, Font.Bold, fontSize * 2))
    g.drawString("GAME OVER", width/4, 1 * getHeightStepSize + padding)
    g.setFont(Font(fontName, Font.Bold, fontSize))
    g.drawString("STANDINGS ", width/4, 3 * getHeightStepSize + padding)
    g.setFont(Font(fontName, Font.Plain, fontSize))
    playerCompletionOrder.zipWithIndex.foreach({
      case (playerName, index) => g.drawString((index + 1).toString + ". " +  playerName, width/4, (index + 4) * getHeightStepSize + padding)
    })
    g.setFont(Font(fontName, Font.Bold, fontSize))
    g.drawString("New game starting in 5 seconds...", width/4, (playerCompletionOrder.size + 5) * getHeightStepSize + padding)
  }

  @inline
  private def getHeightStepSize = height/(playerCompletionOrder.size + 7)

}

case object GameOverPanel {
  val width = 600
  val height = 600
  val backgroundColor = new Color(4,6,84)
  val fontName = "TimesRoman"
  val fontSize = 22
  val padding = 30
}