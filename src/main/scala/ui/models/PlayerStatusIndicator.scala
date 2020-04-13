package ui.models

import java.awt.geom.AffineTransform

import javax.swing.ImageIcon

import scala.swing.{Graphics2D, SimpleSwingApplication}

case class PlayerStatusIndicator(app: SimpleSwingApplication, isCurrentTurn: Boolean) {
  import PlayerStatusIndicator._

  var indicatorImage = if(isCurrentTurn) new ImageIcon(app.resourceFromClassloader("assets/player_assets/real_player_current_turn.png")).getImage
  else new ImageIcon(app.resourceFromClassloader("assets/player_assets/real_player_not_turn.png")).getImage

  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/indicatorImage.getWidth(null)
  val yScale = height * 1.0d/indicatorImage.getHeight(null)

  affineTransform.translate(baseXCoordinate, baseYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(indicatorImage, affineTransform, null)
  }

}


case object PlayerStatusIndicator {
  val width = 40
  val height = 40
  private val baseXCoordinate = 555
  private val baseYCoordinate = 5
}