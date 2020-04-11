package ui.models

import java.awt.geom.AffineTransform

import javax.swing.ImageIcon

import scala.swing.{Graphics2D, SimpleSwingApplication}

case class ComputerPlayer(app: SimpleSwingApplication) {
  import ComputerPlayer._

  val cardImage = new ImageIcon(app.resourceFromClassloader("/assets/player_assets/player_avatar2.png")).getImage
  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/cardImage.getWidth(null)
  val yScale = height * 1.0d/cardImage.getHeight(null)

  affineTransform.translate(cardBaseXCoordinate, cardBaseYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
  }

}

case object ComputerPlayer {
  val width = 150
  val height = 150
  private val cardBaseXCoordinate = 75
  private val cardBaseYCoordinate = 75
}