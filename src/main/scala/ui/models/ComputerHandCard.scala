package ui.models

import java.awt.geom.{AffineTransform}

import game.Card
import javax.swing.ImageIcon
import ui.panels.ComputerPlayerAvatarPanel
import utils.Constants

import scala.swing.{Graphics2D, SimpleSwingApplication}

case object ComputerHandCard {
  val width = 36
  val height = 60
  private val cardBaseXCoordinate = ((ComputerPlayerAvatarPanel.width - width) / 2)
  private val cardBaseYCoordinate = (ComputerPlayerAvatarPanel.height / 2) + 50

  def getTransformedCoordinatesForCardInHandView(baseX: Int, baseY: Int, cardIndexNumber: Int, numberOfCardsInHand: Int): (Double, Double) = {
    val radius = 20 * numberOfCardsInHand/(Constants.sortedHandWithAllCards.size/2)
    val maxAngle = ComputerPlayerAvatarPanel.maxHandSpreadAngle * numberOfCardsInHand/(Constants.sortedHandWithAllCards.size/2)
    val minimumAngle = maxAngle/numberOfCardsInHand
    val currentAngle = (cardIndexNumber * minimumAngle) - maxAngle
    (radius * math.sin(scala.math.toRadians(currentAngle)) + baseX, radius * -scala.math.cos(scala.math.toRadians(currentAngle)) + baseY*.88  - radius)
  }
}

case class ComputerHandCard(card: Card, app: SimpleSwingApplication,
                    cardIndexNumber: Int, numberOfCardsInHand: Int,
                    angle: Double = 0) {
  import ComputerHandCard._

  val transformedCoordinates = getTransformedCoordinatesForCardInHandView(cardBaseXCoordinate, cardBaseYCoordinate, cardIndexNumber, numberOfCardsInHand)
  val cardBeginningXCoordinate =  transformedCoordinates._1
  val cardBeginningYCoordinate = transformedCoordinates._2
  val cardRearImage = new ImageIcon(app.resourceFromClassloader("assets/card_hand_assets/rear/card_back_side.png")).getImage

  var affineTransform = new AffineTransform()

  val xScale = width*1.0d/cardRearImage.getWidth(null)
  val yScale = height*1.0d/cardRearImage.getHeight(null)

  affineTransform.rotate(scala.math.toRadians(angle), cardBeginningXCoordinate, height + cardBeginningYCoordinate)
  affineTransform.translate(cardBeginningXCoordinate, cardBeginningYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardRearImage, affineTransform, null)
  }
}