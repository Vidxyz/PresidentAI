package ui.models

import java.awt.Color
import java.awt.geom.{AffineTransform, Point2D}

import game.Card
import ui.panels.PlayerCardTilePanel
import utils.Consants
import utils.Consants._

import scala.swing.{Graphics2D, Rectangle, SimpleSwingApplication}

case class CardTile(app: SimpleSwingApplication, card: Card, cardIndex: Int, totalNumberOfCards: Int, isSelected: Boolean = false) {
import CardTile._

  val cardImage = getImageResourceForCardTile(card, app, isSelected).getImage
  val transformedCoordinates = getTransformedCoordinatesForCardInTileView(cardIndex, totalNumberOfCards)
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

  def getTransformedCoordinatesForCardInTileView(cardIndex: Int, totalCards: Int): (Int, Int) = {
    val maxRows = PlayerCardTilePanel.maxRows
    val maxColsPerRow = scala.math.ceil(Consants.totalNumberOfCards.toDouble/2/maxRows).toInt
    val rowLimit = scala.math.ceil(totalCards/maxColsPerRow.toDouble).toInt
    val xIndex = cardIndex % maxColsPerRow
    val yIndex = cardIndex / maxColsPerRow
    val staticDistanceX = 17
    val dynamicYDistance = (PlayerCardTilePanel.height - (rowLimit * CardTile.height))/(rowLimit + 1)
    (xIndex * (CardTile.width + staticDistanceX) + staticDistanceX, (yIndex * (CardTile.height + dynamicYDistance)) + dynamicYDistance)
  }
}