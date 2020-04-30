package ui.models

import java.awt.geom.{AffineTransform, Point2D}

import game.Card
import ui.panels.PlayerCardTilePanel
import utils.Constants
import utils.Constants._

import scala.swing.{Graphics2D, Rectangle, SimpleSwingApplication}

case class CardTile(app: SimpleSwingApplication, card: Card, maxRows: Int, cardIndex: Int, totalNumberOfCards: Int, isSelected: Boolean = false) {
import CardTile._

  val cardImage = getImageResourceForCardTile(card, app, isSelected).getImage
  val transformedCoordinates = getTransformedCoordinatesForCardInTileView(cardIndex, totalNumberOfCards, maxRows)
  val cardBeginningXCoordinate =  transformedCoordinates._1
  val cardBeginningYCoordinate = transformedCoordinates._2

  val rectangle = new Rectangle(cardBeginningXCoordinate.toInt, cardBeginningYCoordinate.toInt, width, height)
  var affineTransform = new AffineTransform()

  val xScale = width * 1.0d/cardImage.getWidth(null)
  val yScale = height * 1.0d/cardImage.getHeight(null)

  affineTransform.translate(cardBeginningXCoordinate, cardBeginningYCoordinate)
  affineTransform.scale(xScale, yScale)

  def drawSprite(g: Graphics2D) = {
    g.drawImage(cardImage, affineTransform, null)
//    g.draw(rectangle)
  }

  def pointInside(p: Point2D): Boolean = {
    rectangle.contains(p)
  }
}

case object CardTile {
  val width = 53
  val height = 70

  def getTransformedCoordinatesForCardInTileView(cardIndex: Int, totalCards: Int, maxRows: Int): (Int, Int) = {
    val maxColsPerRow = scala.math.ceil(Constants.totalNumberOfCards.toDouble/2/maxRows).toInt
    val rowLimit = scala.math.ceil(totalCards/maxColsPerRow.toDouble).toInt
    val xIndex = cardIndex % maxColsPerRow
    val yIndex = cardIndex / maxColsPerRow
    val staticDistanceX = 17
    val dynamicYDistance = (PlayerCardTilePanel.height - (rowLimit * CardTile.height))/(rowLimit + 1)
    (xIndex * (CardTile.width + staticDistanceX) + staticDistanceX, (yIndex * (CardTile.height + dynamicYDistance)) + dynamicYDistance)
  }
}