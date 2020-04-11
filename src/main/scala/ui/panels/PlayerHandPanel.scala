package ui.panels

import java.awt.geom.Point2D
import java.awt.{Color, Dimension, Graphics2D}

import game.{GameUtilities, Move}
import player.Player
import ui.layouts.BottomLayout
import ui.models.{CardTile, HandCard}

import scala.swing.event.MousePressed
import scala.swing.{Panel, SimpleSwingApplication}

object PlayerHandPanel {
  val width = 600
  val height = 300
  val maxHandSpreadAngle = 180d
  val maxPossibleCardsInHand = 27
  val backgroundColor = new Color(255, 219, 172)
}

class PlayerHandPanel(app: SimpleSwingApplication, player: Player, parent: BottomLayout) extends Panel {

  import PlayerHandPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
//  border = Swing.LineBorder(Color.BLACK)

  val handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
  val numberOfCardsInHand = handToDisplay.size

  val maxAngle = maxHandSpreadAngle * numberOfCardsInHand/maxPossibleCardsInHand
  val angleList = -numberOfCardsInHand/2 to numberOfCardsInHand/2
  var handCardUiList = (handToDisplay zip angleList).zipWithIndex.map( {
    case ((card, angleIndex), cardIndex) =>
      HandCard(card, app, this, cardIndex, numberOfCardsInHand, angleIndex * (maxAngle / numberOfCardsInHand))
  })

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys)

  reactions += {
    case e: MousePressed => {
      handCardUiList.filter(card => card.pointInside(e.point)).foreach(c => println(c.card))
      handCardUiList = handCardUiList.map(card => if(card.pointInside(e.point)) card.copy(isSelected = !card.isSelected) else card)
      parent.updateCardSelectStatusViaHandCard(handCardUiList)
      repaint()
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    handCardUiList.foreach(h => h.drawSprite(g))
  }

  def highestCardInGivenPoint(p: Point2D): Int = {
    handCardUiList.filter(c => c.rectangle.checkIfContains(p)).map(c => c.rectangle.zVal).max
  }

  def updateCardSelectStatus(cardTileListUi: List[CardTile]) = {
    handCardUiList = (handCardUiList zip cardTileListUi).map({case (handCard, cardTile) => handCard.copy(isSelected = cardTile.isSelected)})
    repaint()
  }

  def setCardsAsSelected(move: Option[Move]) = {
    handCardUiList = handCardUiList.map(e => if(move.get.cards.contains(e.card)) e.copy(isSelected = true) else e.copy(isSelected = false))
    repaint()
  }

}
