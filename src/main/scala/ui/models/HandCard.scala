package ui.models

import java.awt.geom.{AffineTransform, Point2D}

import game.Card
import ui.panels.PlayerHandPanel
import utils.Constants
import utils.Constants._

import scala.swing.{Graphics2D, Rectangle, SimpleSwingApplication}

class Rectangle3d(xVal: Int, yVal: Int, val zVal: Int, width: Int,
                  height: Int, panel: PlayerHandPanel, handCard: HandCard)
  extends Rectangle(xVal, yVal, width, height) {

  def contains3d(p: Point2D): Boolean = {
    checkIfContains(p) && this.zVal == panel.highestCardInGivenPoint(p)
  }

  def checkIfContains(untransformedPoint: Point2D): Boolean = {
    var t = new AffineTransform()
    t.rotate(scala.math.toRadians(handCard.angle), handCard.cardBeginningXCoordinate, height + handCard.cardBeginningYCoordinate)
    val inverseTransform = t.createInverse()
    var p2 = untransformedPoint.clone().asInstanceOf[Point2D]
    inverseTransform.transform(p2, p2)
    this.contains(p2)
  }
}


case object HandCard {
  val width = 120
  val height = 200
  private val cardBaseXCoordinate = (PlayerHandPanel.width - width/2) / 2
  private val cardBaseYCoordinate = 10

  def getTransformedCoordinatesForCardInHandView(baseX: Int, baseY: Int, cardIndexNumber: Int, numberOfCardsInHand: Int): (Double, Double) = {
    val radius = 50 * numberOfCardsInHand/(Constants.sortedHandWithAllCards.size/2)
    val maxAngle = PlayerHandPanel.maxHandSpreadAngle * numberOfCardsInHand/(Constants.sortedHandWithAllCards.size/2)
    val minimumAngle = maxAngle/numberOfCardsInHand
    val currentAngle = (cardIndexNumber * minimumAngle) - maxAngle
    (radius * math.sin(scala.math.toRadians(currentAngle)) + baseX, radius * -scala.math.cos(scala.math.toRadians(currentAngle)) + baseY*2 - radius + 50)
  }
}

case class HandCard(card: Card, app: SimpleSwingApplication, panel: PlayerHandPanel,
                    cardIndexNumber: Int, numberOfCardsInHand: Int,
                    angle: Double = 0, isSelected: Boolean = false) {
  import HandCard._

  val transformedCoordinates = getTransformedCoordinatesForCardInHandView(cardBaseXCoordinate, cardBaseYCoordinate, cardIndexNumber, numberOfCardsInHand)
  val cardBeginningXCoordinate =  transformedCoordinates._1
  val cardBeginningYCoordinate = transformedCoordinates._2
  val cardImage = getImageResourceForCardInHand(card, app, isSelected).getImage

  val rectangle = new Rectangle3d(cardBeginningXCoordinate.toInt, cardBeginningYCoordinate.toInt, cardIndexNumber, width, height, panel, this)
  var affineTransform = new AffineTransform()

  val xScale = width*1.0d/cardImage.getWidth(null)
  val yScale = height*1.0d/cardImage.getHeight(null)

  affineTransform.rotate(scala.math.toRadians(angle), cardBeginningXCoordinate, height + cardBeginningYCoordinate)
  affineTransform.translate(cardBeginningXCoordinate, cardBeginningYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
//    g.rotate(scala.math.toRadians(angle), cardBeginningXCoordinate, height + cardBeginningYCoordinate)
//    g.draw(rectangle)
//    g.rotate(scala.math.toRadians(-angle), cardBeginningXCoordinate, height + cardBeginningYCoordinate)
  }

  def pointInside(p: Point2D): Boolean = {
    rectangle.contains3d(p)
  }

}