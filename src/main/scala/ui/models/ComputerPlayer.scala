package ui.models

import java.awt.Color
import java.awt.geom.AffineTransform

import javax.swing.ImageIcon
import ui.panels.ComputerPlayerAvatarPanel

import scala.swing.{Font, Graphics2D, SimpleSwingApplication}

case class ComputerPlayer(app: SimpleSwingApplication, var hasPassedOnRound: Boolean = false) {
  import ComputerPlayer._

  var cardImage = new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_not_turn.png")).getImage
  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/cardImage.getWidth(null)
  val yScale = height * 1.0d/cardImage.getHeight(null)

  affineTransform.translate(cardBaseXCoordinate, cardBaseYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
    if(hasPassedOnRound) {
      g.setColor(Color.red)
      g.setFont(Font(fontName, Font.Bold, fontSize))
      g.drawString("PASS", 5, 20)
    }
  }

  def updateActivePlayerAvatar(currentTurn: Boolean) = {
    cardImage = if (currentTurn) new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_current_turn.png")).getImage
                else new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_not_turn.png")).getImage
  }

  def updateUserHasPassedOnRound = {
    hasPassedOnRound = true
  }

  def resetUserPassStatus = {
    hasPassedOnRound = false
  }

}

case object ComputerPlayer {
  val width = ComputerPlayerAvatarPanel.width / 2
  val height = ComputerPlayerAvatarPanel.height / 2
  private val cardBaseXCoordinate =  width / 2
  private val cardBaseYCoordinate = 5
  private val fontName = "TimesRoman"
  private val fontSize = 18
}