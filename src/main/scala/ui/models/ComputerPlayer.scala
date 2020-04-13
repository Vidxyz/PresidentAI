package ui.models

import java.awt.geom.AffineTransform

import javax.swing.ImageIcon

import scala.swing.{Graphics2D, SimpleSwingApplication}

case class ComputerPlayer(app: SimpleSwingApplication) {
  import ComputerPlayer._

  var cardImage = new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_not_turn.png")).getImage
  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/cardImage.getWidth(null)
  val yScale = height * 1.0d/cardImage.getHeight(null)

  affineTransform.translate(cardBaseXCoordinate, cardBaseYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
  }

  def updateActivePlayerAvatar(currentTurn: Boolean) = {
    cardImage = if(currentTurn) new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_current_turn.png")).getImage
                else new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_not_turn.png")).getImage
  }

  def setPlayerAvatarToComplete() = {
    cardImage = new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_complete.png")).getImage
  }

  def displayUserHasPassedOnRound() = {
    cardImage = new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar_pass.png")).getImage
  }

}

case object ComputerPlayer {
  val width = 150
  val height = 150
  private val cardBaseXCoordinate = 75
  private val cardBaseYCoordinate = 75
}