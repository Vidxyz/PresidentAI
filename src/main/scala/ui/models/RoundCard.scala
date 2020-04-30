package ui.models

import java.awt.geom.AffineTransform

import game.Card
import ui.panels.CurrentRoundPanel
import utils.Constants._

import scala.swing.{Graphics2D, SimpleSwingApplication}

case class RoundCard(card: Card, app: SimpleSwingApplication, cardIndexNumber: Int) {
  import RoundCard._

  val transformedCoordinates = getTransformedCoordinatesForCardInRoundView(cardBaseXCoordinate, cardBaseYCoordinate, cardIndexNumber)
  val cardBeginningXCoordinate =  transformedCoordinates._1
  val cardBeginningYCoordinate = transformedCoordinates._2
  val cardImage = getImageResourceForCardInHand(card, app, isSelected=false).getImage

  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/cardImage.getWidth(null)
  val yScale = height * 1.0d/cardImage.getHeight(null)

  affineTransform.translate(cardBeginningXCoordinate, cardBeginningYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
  }
}

case object RoundCard {
  val width = 165
  val height = 275
  private val cardBaseXCoordinate = width + 20
  private val cardBaseYCoordinate = (CurrentRoundPanel.height - height)/2

  def getTransformedCoordinatesForCardInRoundView(baseX: Int, baseY: Int, cardIndexNumber: Int): (Int, Int) = {
    (baseX + (25 * cardIndexNumber), baseY)
  }
}